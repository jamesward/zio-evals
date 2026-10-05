package com.jamesward.zio_evals

import zio.*
import zio.schema.*

import java.nio.file.Path
import java.util.Locale
import scala.util.Try

// Workspace evals: the agent works inside a sandboxed `Workspace` (it builds,
// tests, and iterates there), a `WorkspaceVerifier` checks the workspace's
// end state, and failed verifications are fed back to the agent (resuming its
// session) until it passes or runs out of attempts. Efficiency is the total
// over every attempt: wall time and tokens from a blank workspace to verified
// done. Verification and measurement time are recorded separately and are NOT
// part of the agent totals.

// One named check's outcome. `detail` explains a failure (it is shown to the
// agent as feedback) or records what was observed.
final case class CheckOutcome(name: String, passed: Boolean, detail: String = "") derives Schema

final case class Verification(checks: List[CheckOutcome]) derives Schema:
  def passed: Boolean = checks.nonEmpty && checks.forall(_.passed)
  def failures: List[CheckOutcome] = checks.filterNot(_.passed)

object Verification:
  def error(name: String, detail: String): Verification = Verification(List(CheckOutcome(name, passed = false, detail)))

// Inspects a workspace after an agent attempt. A verifier must be honest: any
// check it cannot run counts as failed.
trait WorkspaceVerifier:
  def verify(arm: EvalArm, workspace: Workspace): Task[Verification]

object WorkspaceVerifier:
  // A verifier from deterministic `EvalCheck`s (command/file checks run in the
  // workspace; transcript checks are not applicable and fail).
  def fromChecks(checks: List[EvalCheck], timeout: Duration = 10.minutes): WorkspaceVerifier =
    new WorkspaceVerifier:
      def verify(arm: EvalArm, workspace: Workspace): Task[Verification] =
        ZIO.foreach(checks)(c => Checks.detailedSandboxCheck(c, workspace, timeout)).map(Verification(_))

// A named numeric observation of the finished workspace (lines of code, a
// complexity score, ...). `detail` carries a rationale or breakdown.
final case class Measurement(name: String, value: Double, detail: String = "") derives Schema

// Totals for one sample, summed over attempts. `wallMs` is host-measured time
// spent in the agent runs; `agentDurationMs` is the backend-reported duration.
final case class WorkspaceTotals(
    wallMs:              Long,
    agentDurationMs:     Long,
    turns:               Int,
    toolCalls:           Int,
    inputTokens:         Long,
    outputTokens:        Long,
    cacheReadTokens:     Long,
    cacheCreationTokens: Long,
    costUsd:             Double,
) derives Schema:
  def totalTokens: Long = inputTokens + outputTokens
  def +(r: AgentRunResult, wall: Long): WorkspaceTotals =
    WorkspaceTotals(
      wallMs + wall,
      agentDurationMs + r.latencyMs,
      turns + r.iterations,
      toolCalls + r.toolCalls,
      inputTokens + r.inputTokens,
      outputTokens + r.outputTokens,
      cacheReadTokens + r.cacheReadTokens,
      cacheCreationTokens + r.cacheCreationTokens,
      costUsd + r.costUsd,
    )

object WorkspaceTotals:
  val zero: WorkspaceTotals = WorkspaceTotals(0, 0, 0, 0, 0, 0, 0, 0, 0.0)

final case class WorkspaceAttempt(
    attempt:      Int,
    prompt:       String,
    answer:       String,
    error:        Option[String],
    wallMs:       Long,
    usage:        WorkspaceTotals,
    verification: Verification,
    verifyMs:     Long,
    events:       List[TranscriptEvent],
) derives Schema

final case class WorkspaceSampleResult(
    arm:          String,
    label:        String,
    modelId:      String,
    sample:       Int,
    passed:       Boolean,
    attempts:     List[WorkspaceAttempt],
    totals:       WorkspaceTotals,
    measurements: List[Measurement],
    exportedTo:   Option[String],
) derives Schema:
  def measurement(name: String): Option[Double] = measurements.find(_.name == name).map(_.value)

object WorkspaceSampleResult:
  given CanEqual[WorkspaceSampleResult, WorkspaceSampleResult] = CanEqual.derived

// One configuration under test plus the workspace (image, seed files, env) it
// runs in.
final case class WorkspaceArm(arm: EvalArm, workspace: WorkspaceSpec)

trait WorkspaceObserver:
  def sampleStarted(arm: EvalArm, modelId: String, sample: Int): UIO[Unit]  = ZIO.unit
  def attemptFinished(arm: EvalArm, attempt: WorkspaceAttempt): UIO[Unit]  = ZIO.unit
  def sampleFinished(result: WorkspaceSampleResult): UIO[Unit]             = ZIO.unit

object WorkspaceObserver:
  val noop: WorkspaceObserver = new WorkspaceObserver {}

object WorkspaceEvalRunner:

  final case class Config(
      maxAttempts:  Int = 3,
      agentTimeout: Duration = 60.minutes,
      // Arms x samples run one at a time by default so timing comparisons are
      // not distorted by concurrent builds competing for the machine.
      parallelism:  Int = 1,
      feedback:     Verification => String = defaultFeedback,
      // Measures the finished workspace (after the last attempt, before it is
      // released). Not counted in the totals.
      measure:      (EvalArm, Workspace, Boolean) => Task[List[Measurement]] = (_, _, _) => ZIO.succeed(Nil),
      // Copies the workspace root to this host directory before release.
      exportTo:     Option[(EvalArm, String, Int) => Path] = None,
  )

  def defaultFeedback(v: Verification): String =
    val lines = v.failures.map(f => s"- ${f.name}: ${f.detail.take(4000)}").mkString("\n")
    s"""Verification of your work failed. Failing checks:
       |$lines
       |
       |Fix the problems, re-run your own checks, and reply when everything works.""".stripMargin

  def run(
      spec:      EvalSpec,
      arms:      List[WorkspaceArm],
      modelIds:  List[String],
      samples:   Int,
      agentLoop: AgentLoop,
      sandbox:   Sandbox,
      verifier:  WorkspaceVerifier,
      config:    Config = Config(),
      observer:  WorkspaceObserver = WorkspaceObserver.noop,
  ): Task[List[WorkspaceSampleResult]] =
    val cells =
      for
        m <- modelIds
        a <- arms
        s <- (1 to math.max(1, samples)).toList
      yield (a, m, s)
    ZIO.foreachPar(cells)((a, m, s) => runSample(spec, a, m, s, agentLoop, sandbox, verifier, config, observer))
      .withParallelism(math.max(1, config.parallelism))

  def runSample(
      spec:      EvalSpec,
      wsArm:     WorkspaceArm,
      modelId:   String,
      sample:    Int,
      agentLoop: AgentLoop,
      sandbox:   Sandbox,
      verifier:  WorkspaceVerifier,
      config:    Config,
      observer:  WorkspaceObserver,
  ): Task[WorkspaceSampleResult] =
    val arm = wsArm.arm
    ZIO.scoped {
      for
        _  <- observer.sampleStarted(arm, modelId, sample)
        ws <- sandbox.provision(wsArm.workspace).mapError(_.toThrowable)
        attempts <- attemptLoop(spec, arm, modelId, ws, agentLoop, verifier, config, observer)
        passed    = attempts.lastOption.exists(_.verification.passed)
        measured <- config.measure(arm, ws, passed).catchAllCause(c =>
                      ZIO.logErrorCause(s"measure failed for ${arm.name}", c).as(Nil)
                    )
        exported <- ZIO.foreach(config.exportTo)(f =>
                      val dest = f(arm, modelId, sample)
                      ws.copyOut(".", dest).as(dest.toString).tapError(e => ZIO.logWarning(s"export failed: $e")).option
                    ).map(_.flatten)
        totals = attempts.foldLeft(WorkspaceTotals.zero)((t, a) => sum(t, a.usage))
        result = WorkspaceSampleResult(arm.name, arm.label, modelId, sample, passed, attempts, totals, measured, exported)
        _ <- observer.sampleFinished(result)
      yield result
    }

  private def sum(a: WorkspaceTotals, b: WorkspaceTotals): WorkspaceTotals =
    WorkspaceTotals(
      a.wallMs + b.wallMs, a.agentDurationMs + b.agentDurationMs, a.turns + b.turns, a.toolCalls + b.toolCalls,
      a.inputTokens + b.inputTokens, a.outputTokens + b.outputTokens, a.cacheReadTokens + b.cacheReadTokens,
      a.cacheCreationTokens + b.cacheCreationTokens, a.costUsd + b.costUsd,
    )

  private def msg(t: Throwable): String = Option(t.getMessage).getOrElse(t.getClass.getName)

  private def attemptLoop(
      spec:      EvalSpec,
      arm:       EvalArm,
      modelId:   String,
      ws:        Workspace,
      agentLoop: AgentLoop,
      verifier:  WorkspaceVerifier,
      config:    Config,
      observer:  WorkspaceObserver,
  ): Task[List[WorkspaceAttempt]] =
    def loop(n: Int, prompt: String, session: Option[String], acc: List[WorkspaceAttempt]): Task[List[WorkspaceAttempt]] =
      val request = AgentRunRequest(
        prompt, modelId, arm.mcpServers, arm.policy, arm.skills,
        // A resumed session keeps its original system prompt.
        if session.isEmpty then arm.systemPrompt else None,
        workspace = Some(ws), resumeSessionId = session, timeout = Some(config.agentTimeout),
      )
      for
        start  <- Clock.nanoTime
        outcome <- agentLoop.run(request).either
        end    <- Clock.nanoTime
        wall    = (end - start) / 1000000L
        _      <- outcome.left.toOption.fold(ZIO.unit)(e => ZIO.logWarning(s"agent attempt $n failed for ${arm.name}: ${msg(e)}"))
        vStart <- Clock.nanoTime
        verification <- verifier.verify(arm, ws).catchAllCause(c =>
                          ZIO.logErrorCause("verifier failed", c).as(Verification.error("verifier", c.squash.getMessage))
                        )
        vEnd   <- Clock.nanoTime
        usage   = outcome.fold(_ => WorkspaceTotals.zero.copy(wallMs = wall), r => WorkspaceTotals.zero + (r, wall))
        attempt = WorkspaceAttempt(
                    n, prompt,
                    outcome.fold(_ => "", _.answer),
                    outcome.left.toOption.map(msg),
                    wall, usage, verification, (vEnd - vStart) / 1000000L,
                    outcome.fold(e => List(TranscriptEvent.Note(msg(e))), _.events),
                  )
        _      <- observer.attemptFinished(arm, attempt)
        all     = acc :+ attempt
        // Resume the latest session that produced an id; a failed run (for
        // example a timeout) keeps the previous one.
        nextSession = outcome.toOption.flatMap(_.sessionId).orElse(session)
        result <- if verification.passed || n >= config.maxAttempts || nextSession.isEmpty then ZIO.succeed(all)
                  else loop(n + 1, config.feedback(verification), nextSession, all)
      yield result
    loop(1, arm.effectiveTask(spec.task), None, Nil)

// Summary statistics over a list of samples (for reports).
object WorkspaceStats:
  def mean(xs: Seq[Double]): Double = if xs.isEmpty then 0.0 else xs.sum / xs.size
  def median(xs: Seq[Double]): Double =
    if xs.isEmpty then 0.0
    else
      val s = xs.sorted
      if s.size % 2 == 1 then s(s.size / 2) else (s(s.size / 2 - 1) + s(s.size / 2)) / 2

  def fmtDuration(ms: Double): String =
    val secs = (ms / 1000).round
    if secs >= 60 then f"${secs / 60}%dm${secs % 60}%02ds" else s"${secs}s"

  def fmtTokens(n: Double): String =
    if n >= 1e6 then String.format(Locale.ROOT, "%.2fM", n / 1e6)
    else if n >= 1e3 then String.format(Locale.ROOT, "%.1fk", n / 1e3)
    else n.round.toString

  def parseDouble(s: String): Option[Double] = Try(s.trim.toDouble).toOption
