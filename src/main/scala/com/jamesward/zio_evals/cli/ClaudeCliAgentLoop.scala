package com.jamesward.zio_evals
package cli

import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.process.Command
import zio.schema.*
import zio.schema.annotation.fieldName

import java.io.File
import java.nio.file.Files

// An `AgentLoop` backed by the `claude -p` CLI. MCP servers are isolated with
// --strict-mcp-config. Baseline and judge calls load no setting sources and deny
// Skill; a skills arm stages only its skills in the throwaway project's
// .claude/skills directory, enables only the project setting source, and grants
// only those skill names.
final class ClaudeCliAgentLoop(
    modelOverride: Option[String] = None,
    maxBudgetUsd:  String         = "1.00",
    runTimeout:    Duration       = 120.seconds,
    allowShell:    Boolean        = false,
) extends AgentLoop:

  import ClaudeCliAgentLoop.*

  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    run(prompt, modelId, mcpServers, policy, AgentSkills.none)

  override def run(
      prompt: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
  ): Task[AgentRunResult] =
    runInTemp("arm", modelId, mcpServers, policy, skills, schema = None, prompt).map { parsed =>
      val finalR = parsed.finalResult.get
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
    // Judges are intentionally skill-free.
    runInTemp("judge", modelId, mcpServers, policy, AgentSkills.none, schema = Some(schema), prompt).map { parsed =>
      parsed.finalResult.flatMap(_.structuredOutput).map(_.toJson)
        .orElse(parsed.finalResult.map(_.result))
        .getOrElse("")
    }

  private def effectiveModel(modelId: String): String = modelOverride.getOrElse(modelId)

  // Pure argument construction so permission and isolation behavior is covered
  // without paid calls. --disallowedTools remains last because it is variadic.
  def cliArgs(
      prompt: String,
      model: String,
      mcpConfigPath: Option[String],
      serverNames: List[String],
      web: Boolean,
      schema: Option[Json],
      skillNames: List[String] = Nil,
  ): List[String] =
    val mcpConfigArgs = mcpConfigPath.toList.flatMap(p => List("--mcp-config", p))
    val schemaArgs    = schema.toList.flatMap(s => List("--json-schema", s.toJson))
    val shellTools    = if allowShell then List("Bash") else Nil
    val skillGrants   = skillNames.map(n => s"Skill($n)")
    val allowed       = (if web then webTools else Nil) ++ serverNames.map(n => s"mcp__$n") ++ shellTools ++ skillGrants
    val allowedArgs   = if allowed.isEmpty then Nil else List("--allowedTools") ++ allowed
    val permittedAgentic = shellTools ++ (if skillNames.nonEmpty then List("Skill") else Nil)
    val disallowed    = (disallowedTools ++ (if web then Nil else webTools)).filterNot(permittedAgentic.contains)
    val settingSources = if skillNames.nonEmpty then "project" else ""
    List("-p", prompt, "--setting-sources", settingSources, "--strict-mcp-config", "--output-format", "stream-json", "--verbose", "--max-budget-usd", maxBudgetUsd, "--model", model) ++
      schemaArgs ++ mcpConfigArgs ++ allowedArgs ++ List("--disallowedTools") ++ disallowed

  def cliArgsFor(
      prompt: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      schema: Option[Json] = None,
      skills: AgentSkills = AgentSkills.none,
  ): List[String] =
    val path = if mcpServers.isEmpty then None else Some("/tmp/mcp-config.json")
    cliArgs(prompt, effectiveModel(modelId), path, mcpServers.map(_.name), policy.web, schema, skills.values.map(_.name))

  private def writeMcpConfig(dir: File, servers: List[McpServerConfig]): Task[File] =
    ZIO.attempt {
      val f = File(dir, "mcp-config.json")
      Files.writeString(f.toPath, McpServerConfig.claudeMcpConfigJson(servers))
      f
    }

  private def runClaudeCli(label: String, args: List[String], cwd: File, toolSearch: Boolean): Task[ClaudeStreamJson.Parsed] =
    for
      auth <- activeAuthSource
      _ <- ZIO.logInfo(s"claude CLI request [$label] (auth=$auth, maxBudgetUsd=$maxBudgetUsd, timeout=$runTimeout): claude ${args.mkString(" ")}")
      stdout <- Command("claude", args*)
                  .workingDirectory(cwd)
                  .env(Map(
                    "CLAUDE_CODE_DISABLE_AUTO_MEMORY" -> "1",
                    "ENABLE_TOOL_SEARCH"              -> (if toolSearch then "true" else "false"),
                  ))
                  .string
                  .timeoutFail(RuntimeException(s"claude -p exceeded $runTimeout"))(runTimeout)
                  .tapErrorCause(c => ZIO.logErrorCause(s"claude CLI transport/timeout failed [$label]", c))
      _ <- ZIO.logInfo(s"claude CLI response [$label] (stdout, stream-json):\n$stdout")
      parsed = ClaudeStreamJson.parse(stdout)
      _ <- parsed.finalResult match
             case Some(r) =>
               ZIO.logInfo(
                 s"claude CLI usage [$label]: turns=${r.numTurns} inTok=${r.inputTokens} outTok=${r.outputTokens} " +
                   s"costUsd=${r.totalCostUsd} durationMs=${r.durationMs} toolCalls=${toolCallCount(parsed.events)} isError=${r.isError}"
               )
             case None => ZIO.logError(s"claude CLI produced no result line [$label]; stdout above")
      finalR <- ZIO.fromOption(parsed.finalResult).orElseFail(RuntimeException(s"claude -p produced no result line:\n$stdout"))
      _ <- ZIO.logError(s"claude CLI reported an error [$label]: ${finalR.errorDetail.getOrElse(finalR.result)}").when(finalR.isError)
      _ <- ZIO.fail(RuntimeException(s"claude -p reported an error: ${finalR.errorDetail.getOrElse(finalR.result)}")).when(finalR.isError)
    yield parsed

  private def runInTemp(
      label: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
      schema: Option[Json],
      prompt: String,
  ): Task[ClaudeStreamJson.Parsed] =
    ZIO.scoped {
      for
        cwd <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("agent-eval").toFile)) { dir =>
                 ZIO.attempt(deleteRecursively(dir)).ignoreLogged
               }
        _ <- SkillMaterializer.materialize(skills, File(cwd, ".claude/skills").toPath)
        mcpConfigPath <- if mcpServers.isEmpty then ZIO.none else writeMcpConfig(cwd, mcpServers).map(f => Some(f.getAbsolutePath))
        effectivePrompt = skills.augmentPrompt(prompt)
        args = cliArgs(effectivePrompt, effectiveModel(modelId), mcpConfigPath, mcpServers.map(_.name), policy.web, schema, skills.values.map(_.name))
        parsed <- runClaudeCli(label, args, cwd, policy.toolSearch)
      yield parsed
    }

  private def deleteRecursively(f: File): Unit =
    if f.isDirectory then Option(f.listFiles()).getOrElse(Array.empty[File]).foreach(deleteRecursively)
    f.delete()
    ()

object ClaudeCliAgentLoop:

  val disallowedTools: List[String] =
    List(
      "Bash", "Read", "Write", "Edit", "NotebookEdit", "Glob", "Grep",
      "Task", "TaskCreate", "TaskGet", "TaskList", "TaskOutput", "TaskStop", "TaskUpdate",
      "Skill", "Monitor", "Workflow", "DesignSync", "SendMessage",
      "CronCreate", "CronDelete", "CronList",
      "EnterWorktree", "ExitWorktree", "PushNotification", "RemoteTrigger", "ScheduleWakeup",
    )

  val webTools: List[String] = List("WebFetch", "WebSearch")

  def toolCallCount(events: Seq[TranscriptEvent]): Int =
    events.count { case TranscriptEvent.ToolCall(_, _) => true; case _ => false }

  def apply(
      modelOverride: Option[String] = None,
      maxBudgetUsd: String = "1.00",
      runTimeout: Duration = 120.seconds,
      allowShell: Boolean = false,
  ): ClaudeCliAgentLoop =
    new ClaudeCliAgentLoop(modelOverride, maxBudgetUsd, runTimeout, allowShell)

  val authEnvVars: List[String] = List("ANTHROPIC_API_KEY", "CLAUDE_CODE_OAUTH_TOKEN")

  private val reauthHint =
    "Run `claude auth login` (stored Claude subscription/Console login), or set " +
      "ANTHROPIC_API_KEY / CLAUDE_CODE_OAUTH_TOKEN."

  val isInstalled: UIO[Boolean] =
    Command("claude", "--version").string
      .timeoutFail(RuntimeException("`claude --version` timed out"))(10.seconds)
      .isSuccess

  private final case class AuthStatus(
      loggedIn: Boolean,
      authMethod: Option[String],
      subscriptionType: Option[String],
  ) derives Schema

  private def environmentAuthSources: UIO[List[String]] =
    ZIO.foreach(authEnvVars)(v => zio.System.env(v).orElseSucceed(None).map(_.filter(_.trim.nonEmpty).map(_ => v))).map(_.flatten)

  private def storedAuthStatus: UIO[Option[AuthStatus]] =
    Command("claude", "auth", "status", "--json").string
      .timeoutFail(RuntimeException("`claude auth status` timed out"))(10.seconds)
      .flatMap(out => ZIO.fromEither(EvalCodecs.decode[AuthStatus](out)))
      .option

  val hasCredential: UIO[Boolean] =
    environmentAuthSources.zipWith(storedAuthStatus) { (env, stored) =>
      env.nonEmpty || stored.exists(_.loggedIn)
    }

  private val activeAuthSource: UIO[String] =
    environmentAuthSources.flatMap {
      case env if env.nonEmpty => ZIO.succeed(env.mkString("+"))
      case _ =>
        storedAuthStatus.map {
          case Some(s) if s.loggedIn =>
            val method = s.authMethod.filter(_.nonEmpty).getOrElse("stored")
            val plan   = s.subscriptionType.filter(_.nonEmpty).fold("")(p => s"/$p")
            s"$method$plan"
          case _ => "none"
        }
    }

  private val authCheckTimeout = 60.seconds

  private final case class AuthProbeResult(
      @fieldName("is_error") isError: Boolean,
      result: String,
  ) derives Schema

  val validate: Task[Unit] =
    for
      out <- Command("claude", "-p", "Reply with the single word: ok", "--setting-sources", "", "--strict-mcp-config", "--output-format", "json")
               .string
               .timeoutFail(RuntimeException(s"`claude -p` auth check timed out after $authCheckTimeout"))(authCheckTimeout)
      res <- ZIO.fromEither(EvalCodecs.decode[AuthProbeResult](out))
               .mapError(e => RuntimeException(s"claude -p auth check: could not parse CLI output ($e):\n$out"))
      _ <- ZIO.fail(RuntimeException(s"claude is installed but not working for inference: ${res.result}. $reauthHint"))
               .when(res.isError)
      _ <- ZIO.logInfo("claude authenticated and working")
    yield ()
