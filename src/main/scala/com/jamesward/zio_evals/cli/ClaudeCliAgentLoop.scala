package com.jamesward.zio_evals
package cli

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.process.Command

import java.io.File
import java.nio.file.Files

// An `AgentLoop` backed by the `claude` CLI in headless `-p` mode. The MCP
// servers an arm exposes are surfaced the same way the hosted providers do it:
// over HTTP via `--mcp-config` + `--strict-mcp-config`, so the run sees exactly
// those servers and nothing from the host's `~/.claude.json`. There is NO
// in-process tool bridge — the local `claude` reaches the mounts directly.
// `toolCalls` is read from the parsed transcript (matching the author view).
//
//   * `modelOverride` — when set, pins this model regardless of the per-run
//     `modelId` (e.g. force a tool-search-capable Sonnet, since Claude Code
//     doesn't support MCP tool search on Haiku). When `None`, the per-run
//     `modelId` is used.
//   * `maxBudgetUsd` / `runTimeout` — per-run cost + wall-clock caps.
final class ClaudeCliAgentLoop(
    modelOverride: Option[String] = None,
    maxBudgetUsd:  String         = "1.00",
    runTimeout:    Duration       = 120.seconds,
    allowShell:    Boolean        = false,
) extends AgentLoop:

  import ClaudeCliAgentLoop.*

  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    runInTemp("arm", modelId, mcpServers, policy, schema = None, prompt).map { parsed =>
      val finalR = parsed.finalResult.get // runClaudeCli already failed if absent
      AgentRunResult(
        answer       = finalR.result,
        iterations   = finalR.numTurns,
        toolCalls    = toolCallCount(parsed.events),
        inputTokens  = finalR.inputTokens,
        outputTokens = finalR.outputTokens,
        latencyMs    = finalR.durationMs,
        events       = parsed.events,
      )
    }

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    runInTemp("judge", modelId, mcpServers, policy, schema = Some(schema), prompt).map { parsed =>
      parsed.finalResult.flatMap(_.structuredOutput).map(_.toJson)
        .orElse(parsed.finalResult.map(_.result))
        .getOrElse("")
    }

  private def effectiveModel(modelId: String): String = modelOverride.getOrElse(modelId)

  // The full `claude` argument vector for one run. Pure (no IO) so the
  // permission/allow-list wiring is unit-testable without shelling out:
  //  - web tools are pre-approved (in `--allowedTools`) ONLY when the arm
  //    enables web; when web is off they're LEFT OUT and added to
  //    `--disallowedTools` so the model truly can't reach the web;
  //  - each MCP server is granted server-scoped via `mcp__<name>` and wired
  //    with `--mcp-config`;
  //  - `--json-schema` constrains the final output (the judge);
  //  - local-fs/shell/agentic builtins are stripped via `--disallowedTools`
  //    (which must stay LAST — it's variadic), EXCEPT `Bash` when `allowShell`
  //    is set (so an eval can measure whether the agent chooses the shell — e.g.
  //    running a CLI — vs an MCP tool).
  def cliArgs(prompt: String, model: String, mcpConfigPath: Option[String], serverNames: List[String], web: Boolean, schema: Option[Json]): List[String] =
    val mcpConfigArgs = mcpConfigPath.toList.flatMap(p => List("--mcp-config", p))
    val schemaArgs    = schema.toList.flatMap(s => List("--json-schema", s.toJson))
    val shellTools    = if allowShell then List("Bash") else Nil
    val allowed       = (if web then webTools else Nil) ++ serverNames.map(n => s"mcp__$n") ++ shellTools
    val allowedArgs   = if allowed.isEmpty then Nil else List("--allowedTools") ++ allowed
    val disallowed    = (disallowedTools ++ (if web then Nil else webTools)).filterNot(shellTools.contains)
    List("-p", prompt, "--setting-sources", "", "--strict-mcp-config", "--output-format", "stream-json", "--verbose", "--max-budget-usd", maxBudgetUsd, "--model", model) ++
      schemaArgs ++ mcpConfigArgs ++ allowedArgs ++ List("--disallowedTools") ++ disallowed

  // Package-visible convenience so a fast unit test can assert the permission
  // wiring (per-arm web on/off, per-server grants, --json-schema) without the CLI.
  def cliArgsFor(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Option[Json] = None): List[String] =
    val path = if mcpServers.isEmpty then None else Some("/tmp/mcp-config.json")
    cliArgs(prompt, effectiveModel(modelId), path, mcpServers.map(_.name), policy.web, schema)

  private def writeMcpConfig(dir: File, servers: List[McpServerConfig]): Task[File] =
    ZIO.attempt {
      val f = File(dir, "mcp-config.json")
      Files.writeString(f.toPath, McpServerConfig.claudeMcpConfigJson(servers))
      f
    }

  // Single chokepoint for every real `claude` invocation. Logs the request (full
  // arg vector — the api key is an ENV var, never an arg), the raw stdout, a
  // one-line usage/cost summary, and any transport / no-result / CLI error.
  private def runClaudeCli(label: String, args: List[String], cwd: File, toolSearch: Boolean): Task[ClaudeStreamJson.Parsed] =
    for
      auth   <- activeAuthSource
      _      <- ZIO.logInfo(s"claude CLI request [$label] (auth=$auth, maxBudgetUsd=$maxBudgetUsd, timeout=$runTimeout): claude ${args.mkString(" ")}")
      stdout <- Command("claude", args*)
                  .workingDirectory(cwd)
                  .env(Map(
                    "CLAUDE_CODE_DISABLE_AUTO_MEMORY" -> "1",
                    "ENABLE_TOOL_SEARCH"              -> (if toolSearch then "true" else "false"),
                  ))
                  .string
                  .timeoutFail(RuntimeException(s"claude -p exceeded $runTimeout"))(runTimeout)
                  .tapErrorCause(c => ZIO.logErrorCause(s"claude CLI transport/timeout failed [$label]", c))
      _      <- ZIO.logInfo(s"claude CLI response [$label] (stdout, stream-json):\n$stdout")
      parsed  = ClaudeStreamJson.parse(stdout)
      _      <- parsed.finalResult match
                  case Some(r) =>
                    ZIO.logInfo(
                      s"claude CLI usage [$label]: turns=${r.numTurns} inTok=${r.inputTokens} outTok=${r.outputTokens} " +
                        s"costUsd=${r.totalCostUsd} durationMs=${r.durationMs} toolCalls=${toolCallCount(parsed.events)} isError=${r.isError}"
                    )
                  case None =>
                    ZIO.logError(s"claude CLI produced no result line [$label]; stdout above")
      finalR <- ZIO.fromOption(parsed.finalResult).orElseFail(RuntimeException(s"claude -p produced no result line:\n$stdout"))
      _      <- ZIO.logError(s"claude CLI reported an error [$label]: ${finalR.errorDetail.getOrElse(finalR.result)}").when(finalR.isError)
      _      <- ZIO.fail(RuntimeException(s"claude -p reported an error: ${finalR.errorDetail.getOrElse(finalR.result)}")).when(finalR.isError)
    yield parsed

  // One `claude` invocation in a throwaway temp cwd: write the `--mcp-config`
  // (pointing at the servers over HTTP) when present, then shell out.
  private def runInTemp(label: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Option[Json], prompt: String): Task[ClaudeStreamJson.Parsed] =
    ZIO.scoped {
      for
        cwd           <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("agent-eval").toFile)) { dir =>
                           ZIO.attempt(deleteRecursively(dir)).ignoreLogged
                         }
        mcpConfigPath <- if mcpServers.isEmpty then ZIO.none else writeMcpConfig(cwd, mcpServers).map(f => Some(f.getAbsolutePath))
        args           = cliArgs(prompt, effectiveModel(modelId), mcpConfigPath, mcpServers.map(_.name), policy.web, schema)
        parsed        <- runClaudeCli(label, args, cwd, policy.toolSearch)
      yield parsed
    }

  private def deleteRecursively(f: File): Unit =
    if f.isDirectory then f.listFiles().foreach(deleteRecursively)
    f.delete()
    ()

object ClaudeCliAgentLoop:

  // Tools stripped from EVERY arm via `--disallowedTools` so an arm uses ONLY
  // its intended toolset. Two groups: local-fs / shell builtins (no sandbox
  // here, so they'd hit the host), and Claude Code's built-in AGENTIC tools
  // (subagents, skills, monitors, cron, …) — without these disallowed, a
  // capable model denied web/MCP routes AROUND the missing tools (spawning a
  // `Task` subagent to `curl`, etc.), looping on permission denials and blowing
  // the budget. Over-listing is safe: unknown names are ignored.
  val disallowedTools: List[String] =
    List(
      "Bash", "Read", "Write", "Edit", "NotebookEdit", "Glob", "Grep",
      "Task", "TaskCreate", "TaskGet", "TaskList", "TaskOutput", "TaskStop", "TaskUpdate",
      "Skill", "Monitor", "Workflow", "DesignSync", "SendMessage",
      "CronCreate", "CronDelete", "CronList",
      "EnterWorktree", "ExitWorktree", "PushNotification", "RemoteTrigger", "ScheduleWakeup",
    )

  // Pre-approved (via `--allowedTools`) only on web-enabled arms. In headless
  // `-p` mode the CLI blocks an unlisted-but-available tool with no approver.
  val webTools: List[String] = List("WebFetch", "WebSearch")

  // Every tool invocation (bridged MCP tools AND builtin WebFetch/WebSearch)
  // surfaces as a `ToolCall` transcript event; counting from the transcript is
  // what the author sees. Package-visible for a fast unit test.
  def toolCallCount(events: Seq[TranscriptEvent]): Int =
    events.count { case TranscriptEvent.ToolCall(_, _) => true; case _ => false }

  def apply(
      modelOverride: Option[String] = None,
      maxBudgetUsd:  String         = "1.00",
      runTimeout:    Duration       = 120.seconds,
      allowShell:    Boolean        = false,
  ): ClaudeCliAgentLoop =
    new ClaudeCliAgentLoop(modelOverride, maxBudgetUsd, runTimeout, allowShell)

  // Credentials that authenticate `claude -p`, in the CLI's precedence order
  // (ANTHROPIC_API_KEY wins for `-p`).
  val authEnvVars: List[String] = List("ANTHROPIC_API_KEY", "CLAUDE_CODE_OAUTH_TOKEN")

  private val reauthHint =
    "Set ANTHROPIC_API_KEY (preferred — Console account) or CLAUDE_CODE_OAUTH_TOKEN (`claude setup-token`)."

  // Is the `claude` executable present and runnable? Cheap (`--version`, no
  // tokens, no auth). `false` just means claude isn't a candidate on this box.
  val isInstalled: UIO[Boolean] =
    Command("claude", "--version").string
      .timeoutFail(RuntimeException("`claude --version` timed out"))(10.seconds)
      .isSuccess

  // True iff at least one credential env var is set (non-blank). NOT an auth
  // check (a present-but-invalid token still returns true); `validate` proves
  // the credential actually works.
  val hasCredential: UIO[Boolean] =
    ZIO.foreach(authEnvVars)(v => zio.System.env(v).orElseSucceed(None))
      .map(_.exists(_.exists(_.trim.nonEmpty)))

  // Which credential env var(s) `-p` will authenticate with (the VALUE is never
  // logged — only the name).
  private val activeAuthSource: UIO[String] =
    ZIO.foreach(authEnvVars)(v => zio.System.env(v).orElseSucceed(None).map(_.filter(_.trim.nonEmpty).map(_ => v)))
      .map(_.flatten match { case Nil => "none"; case vs => vs.mkString("+") })

  private val authCheckTimeout = 60.seconds

  private val requireAuth: Task[Unit] =
    ZIO.foreach(authEnvVars)(v => zio.System.env(v)).flatMap { values =>
      ZIO
        .fail(RuntimeException(s"claude inference requires a credential env var (${authEnvVars.mkString(" or ")}), none set. $reauthHint"))
        .unless(values.exists(_.exists(_.trim.nonEmpty)))
        .unit
    }

  private final case class AuthProbeResult(is_error: Boolean, result: String) derives JsonDecoder

  // Does claude actually work for inference? Require a credential, then a
  // minimal `-p` round-trip to confirm it's valid (the only reliable auth
  // check). Costs a few tokens. Fails loudly with an actionable message.
  val validate: Task[Unit] =
    for
      _   <- requireAuth
      out <- Command("claude", "-p", "Reply with the single word: ok", "--setting-sources", "", "--strict-mcp-config", "--output-format", "json")
               .string
               .timeoutFail(RuntimeException(s"`claude -p` auth check timed out after $authCheckTimeout"))(authCheckTimeout)
      res <- ZIO.fromEither(out.fromJson[AuthProbeResult])
               .mapError(e => RuntimeException(s"claude -p auth check: could not parse CLI output ($e):\n$out"))
      _   <- ZIO.fail(RuntimeException(s"claude is installed but not working for inference: ${res.result}. $reauthHint"))
               .when(res.is_error)
      _   <- ZIO.logInfo("claude authenticated and working")
    yield ()
