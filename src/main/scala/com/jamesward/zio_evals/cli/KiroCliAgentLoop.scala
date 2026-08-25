package com.jamesward.zio_evals
package cli

import zio.*
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.process.Command

import java.io.File
import java.nio.file.Files

// An `AgentLoop` backed by the `kiro-cli chat` CLI in headless mode
// (`--no-interactive --trust-all-tools`). The MCP servers an arm exposes are
// surfaced through a throwaway agent config written to `<cwd>/.kiro/agents/
// <agentName>.json` (remote HTTP servers = `{url, headers}`), with `KIRO_HOME`
// pointed at an isolated temp dir so the host's global agents/settings/steering
// don't leak into the run.
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

  // The agent config JSON written to `.kiro/agents/<agentName>.json`. Pure so a
  // unit test can assert the MCP wiring + tool restriction without the CLI.
  // Built-in `tools` is limited to the web tool (only when the arm enables web)
  // so the agent can't reach the host shell/fs; MCP tools stay available via
  // `mcpServers` and are auto-approved through `allowedTools` (`@<server>`).
  def agentConfig(modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Json =
    val webTools     = if policy.web then List("web_fetch") else Nil
    val serverGrants = mcpServers.map(s => s"@${s.name}")
    val base: List[(String, Json)] =
      List(
        "name"         -> Json.Str(agentName),
        "description"  -> Json.Str("zio-evals arm"),
        "tools"        -> Json.Arr(webTools.map(Json.Str.apply)*),
        "allowedTools" -> Json.Arr((webTools ++ serverGrants).map(Json.Str.apply)*),
      )
    val withPrompt = systemPrompt.fold(base)(p => base :+ ("prompt" -> Json.Str(p)))
    val withModel  = effectiveModel(modelId).fold(withPrompt)(m => withPrompt :+ ("model" -> Json.Str(m)))
    val full       = if mcpServers.isEmpty then withModel else withModel :+ ("mcpServers" -> McpServerConfig.kiroAgentMcpJson(mcpServers))
    Json.Obj(full*)

  // The `kiro-cli chat` argument vector. Pure for unit testing.
  def cliArgs(prompt: String, modelId: String): List[String] =
    val modelArgs = effectiveModel(modelId).toList.flatMap(m => List("--model", m))
    List("chat", "--no-interactive", "--trust-all-tools", "--wrap", "never", "--agent", agentName) ++ modelArgs ++ List(prompt)

  private def writeAgentConfig(kiroHome: File, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[Unit] =
    ZIO.attempt {
      // A workspace-local agent under cwd/.kiro/agents takes precedence; we
      // isolate via a temp cwd (KIRO_HOME points elsewhere so global agents
      // don't collide).
      val agentsDir = File(kiroHome, "agents")
      agentsDir.mkdirs()
      Files.writeString(File(agentsDir, s"$agentName.json").toPath, agentConfig(modelId, mcpServers, policy).toJson)
      ()
    }

  private def runInTemp(label: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, prompt: String): Task[(String, Long)] =
    ZIO.scoped {
      for
        cwd      <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("kiro-eval").toFile))(dir => ZIO.attempt(deleteRecursively(dir)).ignoreLogged)
        kiroHome  = File(cwd, ".kiro")
        _        <- ZIO.attempt(kiroHome.mkdirs())
        _        <- writeAgentConfig(kiroHome, modelId, mcpServers, policy)
        args      = cliArgs(prompt, modelId)
        _        <- ZIO.logInfo(s"kiro-cli request [$label] (timeout=$runTimeout): kiro-cli ${args.mkString(" ")}")
        start    <- Clock.nanoTime
        stdout   <- Command("kiro-cli", args*)
                      .workingDirectory(cwd)
                      .env(Map("KIRO_HOME" -> kiroHome.getAbsolutePath, "KIRO_LOG_NO_COLOR" -> "1"))
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
