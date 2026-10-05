package com.jamesward.zio_evals

import zio.*
import zio.json.ast.Json
import zio.test.*

import java.nio.charset.StandardCharsets

// An in-memory workspace: files are a map, `run` understands `cat <path>`.
final class MemoryWorkspace(files: Ref[Map[String, Array[Byte]]]) extends Workspace:
  def writeFile(path: String, bytes: Array[Byte]): IO[SandboxError, Unit] = files.update(_ + (path -> bytes))
  def readFile(path: String): IO[SandboxError, Array[Byte]] =
    files.get.flatMap(fs => ZIO.fromOption(fs.get(path)).orElseFail(SandboxError.ExecutionFailed(s"no $path")))
  def run(command: String, timeout: Duration): IO[SandboxError, ExecResult] =
    command.split(" ").toList match
      case "cat" :: path :: Nil =>
        readFile(path).map(b => ExecResult(String(b, StandardCharsets.UTF_8), "", 0)).catchAll(e => ZIO.succeed(ExecResult("", e.toString, 1)))
      case _ => ZIO.succeed(ExecResult("", s"unknown: $command", 127))

final class MemorySandbox(val provisioned: Ref[Int]) extends Sandbox:
  def provision(spec: WorkspaceSpec): ZIO[Scope, SandboxError, Workspace] =
    provisioned.update(_ + 1) *> Ref.make(spec.seedFiles).map(MemoryWorkspace(_))

// Writes `done.txt` only on the Nth call, recording every request it saw.
final class ScriptedAgent(succeedOnCall: Int, seen: Ref[List[AgentRunRequest]]) extends AgentLoop:
  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    ZIO.fail(RuntimeException("workspace only"))
  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    ZIO.fail(RuntimeException("unused"))
  override def run(request: AgentRunRequest): Task[AgentRunResult] =
    for
      calls <- seen.updateAndGet(_ :+ request).map(_.size)
      ws    <- ZIO.fromOption(request.workspace).orElseFail(RuntimeException("no workspace"))
      _     <- ws.writeFile("done.txt", "yes".getBytes(StandardCharsets.UTF_8)).mapError(_.toThrowable).when(calls >= succeedOnCall)
    yield AgentRunResult(s"call $calls", 2, 1, 100, 10, 50, Nil, cacheReadTokens = 80, costUsd = 0.5, sessionId = Some("s1"))

object WorkspaceEvalRunnerSpec extends ZIOSpecDefault:

  private val spec0    = EvalSpec("build it", "n/a")
  private val arm      = WorkspaceArm(EvalArm.modelOnly("a", "A"), WorkspaceSpec())
  private val verifier = WorkspaceVerifier.fromChecks(List(EvalCheck.CommandOutputMatches("cat done.txt", "yes")))

  def spec = suite("WorkspaceEvalRunner")(
    test("feeds verification failures back, resumes the session, and sums usage over attempts") {
      for
        seen    <- Ref.make(List.empty[AgentRunRequest])
        prov    <- Ref.make(0)
        results <- WorkspaceEvalRunner.run(spec0, List(arm), List("m"), 1, ScriptedAgent(2, seen), MemorySandbox(prov), verifier,
                     WorkspaceEvalRunner.Config(maxAttempts = 3))
        reqs    <- seen.get
        n       <- prov.get
        r        = results.head
      yield assertTrue(
        n == 1,
        r.passed,
        r.attempts.size == 2,
        !r.attempts.head.verification.passed,
        r.attempts(1).verification.passed,
        reqs.head.resumeSessionId.isEmpty,
        reqs.head.prompt == "build it",
        reqs(1).resumeSessionId.contains("s1"),
        reqs(1).prompt.contains("Verification of your work failed"),
        r.totals.inputTokens == 200L,
        r.totals.outputTokens == 20L,
        r.totals.cacheReadTokens == 160L,
        r.totals.costUsd == 1.0,
        r.totals.agentDurationMs == 100L,
      )
    },
    test("stops after maxAttempts and reports failure") {
      for
        seen    <- Ref.make(List.empty[AgentRunRequest])
        prov    <- Ref.make(0)
        results <- WorkspaceEvalRunner.run(spec0, List(arm), List("m"), 1, ScriptedAgent(99, seen), MemorySandbox(prov), verifier,
                     WorkspaceEvalRunner.Config(maxAttempts = 2))
      yield assertTrue(!results.head.passed, results.head.attempts.size == 2)
    },
    test("measurements are taken from the finished workspace") {
      for
        seen    <- Ref.make(List.empty[AgentRunRequest])
        prov    <- Ref.make(0)
        cfg      = WorkspaceEvalRunner.Config(measure = (_, ws, passed) =>
                     ws.readFile("done.txt").map(b => List(Measurement("bytes", b.length.toDouble, s"passed=$passed"))).mapError(_.toThrowable))
        results <- WorkspaceEvalRunner.run(spec0, List(arm), List("m"), 1, ScriptedAgent(1, seen), MemorySandbox(prov), verifier, cfg)
      yield assertTrue(results.head.measurement("bytes").contains(3.0))
    },
    test("an agent without workspace support fails honestly through the default run(request)") {
      for
        prov    <- Ref.make(0)
        results <- WorkspaceEvalRunner.run(spec0, List(arm), List("m"), 1, FakeAgentLoop(), MemorySandbox(prov), verifier,
                     WorkspaceEvalRunner.Config(maxAttempts = 3))
        r        = results.head
      yield assertTrue(!r.passed, r.attempts.size == 1, r.attempts.head.error.exists(_.contains("sandbox workspace")))
    },
  )
