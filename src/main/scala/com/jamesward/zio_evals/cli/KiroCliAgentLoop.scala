package com.jamesward.zio_evals
package cli

import zio.*
import zio.json.ast.Json
import zio.process.{Command, ProcessInput}
import zio.schema.*

import java.io.File
import java.nio.file.Files

// The workspace-local `.kiro/agents/<name>.json` wire format. Optional model /
// prompt values, resources, and the MCP server map are encoded by Schema.
final case class KiroAgentConfig(
    name:           String,
    description:    String,
    tools:          List[String],
    allowedTools:   List[String],
    resources:      List[String],
    includeMcpJson: Boolean,
    model:          Option[String],
    prompt:         Option[String],
    mcpServers:     Map[String, McpServerConfig.KiroMcpServer],
) derives Schema

// An `AgentLoop` backed by `kiro-cli chat` in headless mode. Every invocation
// runs in a fresh workspace with a custom agent. The workspace setting disables
// inherited default resources so global/workspace steering and ~/.kiro/skills
// cannot contaminate a baseline; an arm's materialized skills are then listed
// explicitly as `skill://` resources. The REAL environment remains available so
// the operator's stored Kiro credentials still authenticate.
final class KiroCliAgentLoop(
    modelOverride: Option[String] = None,
    runTimeout:    Duration       = 180.seconds,
    agentName:     String         = "eval",
    systemPrompt:  Option[String] = None,
) extends AgentLoop:

  import KiroCliAgentLoop.*

  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    run(prompt, modelId, mcpServers, policy, AgentSkills.none)

  override def run(
      prompt: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
  ): Task[AgentRunResult] =
    runInTemp("arm", modelId, mcpServers, policy, skills, prompt).flatMap { (stdout, ms) =>
      ZIO.fromEither(KiroStreamJson.finalText(stdout)).mapError(RuntimeException(_)).map { answer =>
        AgentRunResult(answer, iterations = 0, toolCalls = 0, inputTokens = 0, outputTokens = 0, latencyMs = ms, events = List(TranscriptEvent.AgentMessage(answer)))
      }
    }

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    // Judges are intentionally skill-free.
    runInTemp("judge", modelId, mcpServers, policy, AgentSkills.none, prompt).flatMap { (stdout, _) =>
      ZIO.fromEither(KiroStreamJson.finalText(stdout)).mapError(RuntimeException(_)).map(EvalJudging.sliceJson)
    }

  private def effectiveModel(modelId: String): Option[String] =
    modelOverride.orElse(Option(modelId).map(_.trim).filter(_.nonEmpty))

  // Pure so tests can assert MCP/tool/resource isolation without shelling out.
  def agentConfig(
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      resources: List[String] = Nil,
  ): KiroAgentConfig =
    val webTools     = if policy.web then List("web_fetch") else Nil
    val serverGrants = mcpServers.map(s => s"@${s.name}")
    KiroAgentConfig(
      name           = agentName,
      description    = "zio-evals arm",
      tools          = webTools ++ serverGrants,
      allowedTools   = webTools ++ serverGrants,
      resources      = resources,
      includeMcpJson = false,
      model          = effectiveModel(modelId),
      prompt         = systemPrompt,
      mcpServers     = McpServerConfig.kiroMcpServers(mcpServers),
    )

  def cliArgs(prompt: String, modelId: String, requireMcpStartup: Boolean): List[String] =
    val modelArgs = effectiveModel(modelId).toList.flatMap(m => List("--model", m))
    val mcpArgs   = if requireMcpStartup then List("--require-mcp-startup") else Nil
    List("chat", "--agent-engine", "v2", "--output-format", "stream-json", "--wrap", "never") ++ mcpArgs ++ List("--agent", agentName) ++ modelArgs ++ List(prompt)

  private[cli] def skillResourceUris(
      materialized: List[MaterializedSkill],
      activation: SkillActivation,
  ): List[String] =
    val scheme = activation match
      case SkillActivation.Available => "skill://"
      case SkillActivation.Explicit  => "file://"
    materialized.map(s => s"$scheme${s.directory.resolve("SKILL.md").toAbsolutePath}")

  private def writeWorkspaceIsolation(cwd: File): Task[Unit] =
    ZIO.attemptBlocking {
      val settingsDir = File(cwd, ".kiro/settings")
      settingsDir.mkdirs()
      Files.writeString(
        File(settingsDir, "cli.json").toPath,
        """{"chat.disableInheritingDefaultResources":true}""",
      )
      ()
    }

  private def writeAgentConfig(
      cwd: File,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      resources: List[String],
  ): Task[Unit] =
    ZIO.attemptBlocking {
      val agentsDir = File(cwd, ".kiro/agents")
      agentsDir.mkdirs()
      Files.writeString(File(agentsDir, s"$agentName.json").toPath, EvalCodecs.encode(agentConfig(modelId, mcpServers, policy, resources)))
      ()
    }

  private def runInTemp(
      label: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
      prompt: String,
  ): Task[(String, Long)] =
    ZIO.scoped {
      for
        cwd <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("kiro-eval").toFile))(dir => ZIO.attempt(deleteRecursively(dir)).ignoreLogged)
        materialized <- SkillMaterializer.materialize(skills, File(cwd, ".kiro/skills").toPath)
        resources = skillResourceUris(materialized, skills.activation)
        _ <- writeWorkspaceIsolation(cwd)
        _ <- writeAgentConfig(cwd, modelId, mcpServers, policy, resources)
        args = cliArgs(prompt, modelId, requireMcpStartup = mcpServers.nonEmpty)
        _ <- ZIO.logInfo(s"kiro-cli request [$label] (timeout=$runTimeout): kiro-cli ${args.mkString(" ")}")
        start <- Clock.nanoTime
        stdout <- Command("kiro-cli", args*)
                    .workingDirectory(cwd)
                    .stdin(ProcessInput.fromUTF8String(""))
                    .redirectErrorStream(true)
                    .env(Map("KIRO_LOG_NO_COLOR" -> "1"))
                    .string
                    .timeoutFail(RuntimeException(s"kiro-cli chat exceeded $runTimeout"))(runTimeout)
                    .tapErrorCause(c => ZIO.logErrorCause(s"kiro-cli transport/timeout failed [$label]", c))
        end <- Clock.nanoTime
        _ <- ZIO.logInfo(s"kiro-cli response [$label]: stream-json bytes=${stdout.length}")
      yield (stdout, (end - start) / 1000000L)
    }

  private def deleteRecursively(f: File): Unit =
    if f.isDirectory then Option(f.listFiles()).getOrElse(Array.empty[File]).foreach(deleteRecursively)
    f.delete()
    ()

object KiroCliAgentLoop:

  def apply(
      modelOverride: Option[String] = None,
      runTimeout: Duration = 180.seconds,
      agentName: String = "eval",
      systemPrompt: Option[String] = None,
  ): KiroCliAgentLoop =
    new KiroCliAgentLoop(modelOverride, runTimeout, agentName, systemPrompt)

  val isInstalled: UIO[Boolean] =
    Command("kiro-cli", "--version").string
      .timeoutFail(RuntimeException("`kiro-cli --version` timed out"))(10.seconds)
      .isSuccess

  val isAuthenticated: UIO[Boolean] =
    Command("kiro-cli", "user", "whoami", "--format", "json").exitCode
      .map(_.code == 0)
      .catchAll(_ => ZIO.succeed(false))

  val validate: Task[Unit] =
    for
      installed <- isInstalled
      _ <- ZIO.fail(RuntimeException("kiro-cli is not installed or not runnable")).unless(installed)
      authed <- isAuthenticated
      _ <- ZIO.fail(RuntimeException("kiro-cli is installed but not authenticated (`kiro-cli user whoami` failed); log in first")).unless(authed)
      _ <- ZIO.logInfo("kiro-cli authenticated and working")
    yield ()
