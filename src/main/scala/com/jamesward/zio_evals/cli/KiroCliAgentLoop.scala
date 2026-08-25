package com.jamesward.zio_evals
package cli

import zio.*
import zio.json.ast.Json
import zio.process.{Command, ProcessInput}
import zio.schema.*

import java.io.File
import java.nio.file.Files


// The workspace-local `.kiro/agents/<name>.json` wire format. Optional model /
// prompt values and the MCP server map are encoded by the derived `Schema`.
final case class KiroAgentConfig(
    name:           String,
    description:    String,
    tools:          List[String],
    allowedTools:   List[String],
    includeMcpJson: Boolean,
    model:          Option[String],
    prompt:         Option[String],
    mcpServers:     Map[String, McpServerConfig.KiroMcpServer],
) derives Schema
// An `AgentLoop` backed by the `kiro-cli chat` CLI in headless mode
// (`--no-interactive --trust-all-tools`). The MCP servers an arm exposes are
// surfaced through a throwaway agent config written to `<cwd>/.kiro/agents/
// <agentName>.json` (remote HTTP servers = `{url, headers}`) in a temp working
// directory — a workspace-local agent takes precedence over global ones. The
// process runs with the REAL environment (NOT an isolated `KIRO_HOME`) so
// kiro-cli's stored credentials under `~/.kiro` still authenticate; the agent
// config sets `"includeMcpJson": false` so the host's global
// `~/.kiro/settings/mcp.json` servers don't leak into the run. When the arm
// exposes MCP servers, `--require-mcp-startup` makes chat exit non-zero (before
// any paid model call) if a server fails to connect.
//
// LIMITATION vs the claude backend: kiro-cli's headless output is plain text
// with no structured token/turn accounting and no output-schema constraint. So
// `AgentRunResult` reports `latencyMs` (measured here) but `iterations`,
// `toolCalls`, and token counts as 0, and `events` is a single `AgentMessage`
// holding the answer. `runStructured` asks for the JSON shape in the prompt and
// returns the sliced-out JSON (the judge parser is lenient).
final class KiroCliAgentLoop(
    modelOverride: Option[String]  = None,
    runTimeout:    Duration        = 180.seconds,
    agentName:     String          = "eval",
    systemPrompt:  Option[String]  = None,
) extends AgentLoop:

  import KiroCliAgentLoop.*

  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    runInTemp("arm", modelId, mcpServers, policy, prompt).map { (stdout, ms) =>
      val answer = stdout.trim
      AgentRunResult(answer, iterations = 0, toolCalls = 0, inputTokens = 0, outputTokens = 0, latencyMs = ms, events = List(TranscriptEvent.AgentMessage(answer)))
    }

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    // No CLI-level schema constraint; the caller's prompt already requests the
    // JSON shape. Return the sliced JSON so a chatty answer still parses.
    runInTemp("judge", modelId, mcpServers, policy, prompt).map((stdout, _) => EvalJudging.sliceJson(stdout))

  private def effectiveModel(modelId: String): Option[String] =
    modelOverride.orElse(Option(modelId).map(_.trim).filter(_.nonEmpty))

  // The typed agent config written to `<cwd>/.kiro/agents/<agentName>.json`.
  // Pure so tests can assert the MCP wiring + tool restriction without parsing
  // an untyped JSON AST. `includeMcpJson = false` keeps global MCP servers out.
  def agentConfig(modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): KiroAgentConfig =
    val webTools     = if policy.web then List("web_fetch") else Nil
    val serverGrants = mcpServers.map(s => s"@${s.name}")
    KiroAgentConfig(
      name           = agentName,
      description    = "zio-evals arm",
      tools          = webTools ++ serverGrants,
      allowedTools   = webTools ++ serverGrants,
      includeMcpJson = false,
      model          = effectiveModel(modelId),
      prompt         = systemPrompt,
      mcpServers     = McpServerConfig.kiroMcpServers(mcpServers),
    )

  // The `kiro-cli chat` argument vector. Pure for unit testing.
  // `--require-mcp-startup` (only when the arm exposes MCP servers) makes chat
  // exit non-zero if a server fails to connect, before any paid model call.
  def cliArgs(prompt: String, modelId: String, requireMcpStartup: Boolean): List[String] =
    val modelArgs   = effectiveModel(modelId).toList.flatMap(m => List("--model", m))
    val mcpArgs     = if requireMcpStartup then List("--require-mcp-startup") else Nil
    List("chat", "--no-interactive", "--trust-all-tools", "--wrap", "never") ++ mcpArgs ++ List("--agent", agentName) ++ modelArgs ++ List(prompt)

  private def writeAgentConfig(cwd: File, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[Unit] =
    ZIO.attempt {
      // A workspace-local agent under cwd/.kiro/agents takes precedence over the
      // user's global agents, so we isolate the arm here WITHOUT relocating
      // `KIRO_HOME` (which would also move kiro-cli's stored credentials).
      val agentsDir = File(cwd, ".kiro/agents")
      agentsDir.mkdirs()
      Files.writeString(File(agentsDir, s"$agentName.json").toPath, EvalCodecs.encode(agentConfig(modelId, mcpServers, policy)))
      ()
    }

  private def runInTemp(label: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, prompt: String): Task[(String, Long)] =
    ZIO.scoped {
      for
        cwd      <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("kiro-eval").toFile))(dir => ZIO.attempt(deleteRecursively(dir)).ignoreLogged)
        _        <- writeAgentConfig(cwd, modelId, mcpServers, policy)
        args      = cliArgs(prompt, modelId, requireMcpStartup = mcpServers.nonEmpty)
        _        <- ZIO.logInfo(s"kiro-cli request [$label] (timeout=$runTimeout): kiro-cli ${args.mkString(" ")}")
        start    <- Clock.nanoTime
        // Keep the REAL environment (so `~/.kiro` credentials authenticate);
        // merge stderr into stdout and feed empty stdin so headless mode can't
        // block waiting for input. `KIRO_LOG_NO_COLOR` keeps the captured text clean.
        stdout   <- Command("kiro-cli", args*)
                      .workingDirectory(cwd)
                      .stdin(ProcessInput.fromUTF8String(""))
                      .redirectErrorStream(true)
                      .env(Map("KIRO_LOG_NO_COLOR" -> "1"))
                      .string
                      .timeoutFail(RuntimeException(s"kiro-cli chat exceeded $runTimeout"))(runTimeout)
                      .tapErrorCause(c => ZIO.logErrorCause(s"kiro-cli transport/timeout failed [$label]", c))
        end      <- Clock.nanoTime
        _        <- ZIO.logInfo(s"kiro-cli response [$label] (stdout):\n$stdout")
      yield (stdout, (end - start) / 1000000L)
    }

  private def deleteRecursively(f: File): Unit =
    if f.isDirectory then f.listFiles().foreach(deleteRecursively)
    f.delete()
    ()

object KiroCliAgentLoop:

  def apply(
      modelOverride: Option[String] = None,
      runTimeout:    Duration       = 180.seconds,
      agentName:     String         = "eval",
      systemPrompt:  Option[String] = None,
  ): KiroCliAgentLoop =
    new KiroCliAgentLoop(modelOverride, runTimeout, agentName, systemPrompt)

  // Is the `kiro-cli` executable present and runnable? Cheap, no paid call.
  val isInstalled: UIO[Boolean] =
    Command("kiro-cli", "--version").string
      .timeoutFail(RuntimeException("`kiro-cli --version` timed out"))(10.seconds)
      .isSuccess

  // Is kiro-cli authenticated? `kiro-cli user whoami --format json` exits 0 with
  // a JSON identity when a credential is active, and fails otherwise. Free (no
  // model call), so it's the gate before a paid `kiro-cli chat`.
  val isAuthenticated: UIO[Boolean] =
    Command("kiro-cli", "user", "whoami", "--format", "json").exitCode
      .map(_.code == 0)
      .catchAll(_ => ZIO.succeed(false))

  // Installed AND authenticated — a paid `chat` can run. Fails loudly otherwise.
  val validate: Task[Unit] =
    for
      installed <- isInstalled
      _         <- ZIO.fail(RuntimeException("kiro-cli is not installed or not runnable")).unless(installed)
      authed    <- isAuthenticated
      _         <- ZIO.fail(RuntimeException("kiro-cli is installed but not authenticated (`kiro-cli user whoami` failed); log in first")).unless(authed)
      _         <- ZIO.logInfo("kiro-cli authenticated and working")
    yield ()
