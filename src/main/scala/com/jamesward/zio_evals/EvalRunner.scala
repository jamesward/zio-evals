package com.jamesward.zio_evals

import zio.*

// Mean efficiency metrics for one arm/model, averaged over the run's samples.
final case class ArmMetrics(
    iterations:   Double,
    toolCalls:    Double,
    inputTokens:  Double,
    outputTokens: Double,
    latencyMs:    Double,
)

object ArmMetrics:
  val zero: ArmMetrics = ArmMetrics(0, 0, 0, 0, 0)

  def avg(results: List[AgentRunResult]): ArmMetrics =
    if results.isEmpty then zero
    else
      def m(f: AgentRunResult => Double): Double = results.map(f).sum / results.size
      ArmMetrics(
        m(_.iterations.toDouble),
        m(_.toolCalls.toDouble),
        m(_.inputTokens.toDouble),
        m(_.outputTokens.toDouble),
        m(_.latencyMs.toDouble),
      )

// The scored outcome of one eval, in one arm, for one model. Samples are
// aggregated into `passRate`; `verdict`/`rationale` come from the judge. The
// full per-sample transcript (with each sample's verdict filled in) rides along
// in `samples` so a host can persist it. NO persistence identity here — a host
// attaches its own ids/timestamps when it saves.
final case class ArmResult(
    arm:          EvalArm,
    modelId:      String,
    verdict:      EvalVerdict,
    passRate:     Double,
    score:        Double,
    checksPassed: Boolean,
    rationale:    String,
    metrics:      ArmMetrics,
    samples:      List[TranscriptSample],
)

// Live progress hooks so a host can persist incrementally (create a pending
// cell, flip it running, write the result) while the run proceeds. All default
// to no-ops; a host overrides only what it needs. The runner ALSO returns the
// full `List[ArmResult]`, so a host that only wants the final results can
// ignore the observer entirely (the integration-test use case).
trait EvalObserver:
  def armStarted(arm: EvalArm, modelId: String): UIO[Unit]                        = ZIO.unit
  def armCompleted(result: ArmResult): UIO[Unit]                                  = ZIO.unit
  def judgeStarted(modelId: String): UIO[Unit]                                    = ZIO.unit
  def judgeCompleted(modelId: String, events: List[TranscriptEvent]): UIO[Unit]   = ZIO.unit

object EvalObserver:
  val noop: EvalObserver = new EvalObserver {}

// Drives an eval across arms x models x samples through an `AgentLoop`, grades
// each sample's arms together with a `Judge`, and aggregates. Pure engine: no
// DataSource, no HTTP, no host types. Suitable both for an app worker (persist
// via `EvalObserver`) and for an integration test (assert on the returned
// `ArmResult`s). Deterministic `EvalCheck`s that are transcript/answer-based are
// enforced here; command/file checks need a `Sandbox` the host wires (see
// `Checks`), so they are not evaluated by this in-process runner.
object EvalRunner:

  // One raw sample from an arm before the judge grades it.
  private final case class RawSample(result: AgentRunResult, error: Option[String])

  private def msg(t: Throwable): String = Option(t.getMessage).getOrElse(t.getClass.getName)

  def run(
      spec:      EvalSpec,
      arms:      List[EvalArm],
      modelIds:  List[String],
      samples:   Int,
      agentLoop: AgentLoop,
      judge:     Judge,
      observer:  EvalObserver = EvalObserver.noop,
  ): Task[List[ArmResult]] =
    ZIO.foreach(modelIds)(m => runCell(spec, arms, m, math.max(1, samples), agentLoop, judge, observer)).map(_.flatten)

  // All arms for ONE model: run every arm's samples, then judge each sample's
  // arms together, then aggregate per arm.
  def runCell(
      spec:      EvalSpec,
      arms:      List[EvalArm],
      modelId:   String,
      samples:   Int,
      agentLoop: AgentLoop,
      judge:     Judge,
      observer:  EvalObserver = EvalObserver.noop,
  ): Task[List[ArmResult]] =
    for
      _         <- ZIO.foreachDiscard(arms)(a => observer.armStarted(a, modelId))
      rawByArm  <- ZIO.foreachPar(arms)(a => runArmSamples(spec, a, modelId, samples, agentLoop).map(a -> _))
      _         <- observer.judgeStarted(modelId)
      perSample <- ZIO.foreach((0 until samples).toList)(i => judgeSample(spec, rawByArm, i, judge))
      byArm      = perSample.map(_._1)
      judgeEvts  = perSample.zipWithIndex.flatMap { case ((_, evs), i) =>
                     (if samples > 1 then List(TranscriptEvent.Note(s"sample ${i + 1}")) else Nil) ++ evs
                   }
      _         <- observer.judgeCompleted(modelId, judgeEvts)
      results   <- ZIO.foreach(rawByArm)((arm, raws) => aggregate(spec, arm, modelId, raws, byArm, observer))
    yield results

  private def runArmSamples(spec: EvalSpec, arm: EvalArm, modelId: String, samples: Int, agentLoop: AgentLoop): Task[List[RawSample]] =
    ZIO.foreach((0 until samples).toList) { _ =>
      agentLoop
        .run(spec.task, modelId, arm.mcpServers, arm.policy)
        .tapErrorCause(c => ZIO.logErrorCause(s"eval arm '${arm.name}' failed (model=$modelId)", c))
        .fold(
          e => RawSample(AgentRunResult("", 0, 0, 0, 0, 0, List(TranscriptEvent.Note(msg(e)))), Some(msg(e))),
          r => RawSample(r, None),
        )
    }

  // Judge the arms that produced a non-empty answer for sample `i`. Returns the
  // per-arm (verdict, rationale) keyed by arm name, plus the judge's events.
  private def judgeSample(
      spec:     EvalSpec,
      rawByArm: List[(EvalArm, List[RawSample])],
      i:        Int,
      judge:    Judge,
  ): Task[(Map[String, (EvalVerdict, String)], List[TranscriptEvent])] =
    val answers = rawByArm.flatMap { (arm, raws) =>
      raws.lift(i).filter(_.error.isEmpty).map(r => arm -> r.result.answer).filter(_._2.nonEmpty)
    }
    if answers.isEmpty then ZIO.succeed((Map.empty, Nil))
    else judge.judge(spec, answers).map(jo => (answers.map(_._1.name).zip(jo.verdicts).toMap, jo.events))

  private def aggregate(
      spec:     EvalSpec,
      arm:      EvalArm,
      modelId:  String,
      raws:     List[RawSample],
      byArm:    List[Map[String, (EvalVerdict, String)]],
      observer: EvalObserver,
  ): Task[ArmResult] =
    val graded    = byArm.flatMap(_.get(arm.name))
    val passRate  = if graded.isEmpty then 0.0 else graded.count(_._1 == EvalVerdict.Pass).toDouble / graded.size
    val verdict   =
      if graded.exists(_._1 == EvalVerdict.Error) && passRate == 0.0 then EvalVerdict.Error
      else if passRate >= 0.5 then EvalVerdict.Pass
      else EvalVerdict.Fail
    val rationale = graded.lastOption.map(_._2).getOrElse("")
    val tsamples  = raws.zipWithIndex.map { (raw, i) =>
      val (v, r) = byArm.lift(i).flatMap(_.get(arm.name)).getOrElse((EvalVerdict.Error, raw.error.getOrElse("")))
      TranscriptSample(arm.name, modelId, raw.result.events, if raw.error.isEmpty then raw.result.answer else "", v, r)
    }
    val okResults    = raws.filter(_.error.isEmpty).map(_.result)
    val checksPassed = okResults.nonEmpty && okResults.forall(r => spec.checks.flatMap(c => Checks.transcriptCheck(c, r)).forall(identity))
    val result = ArmResult(
      arm, modelId, verdict, passRate,
      score = if verdict == EvalVerdict.Pass then 1.0 else 0.0,
      checksPassed, rationale, ArmMetrics.avg(okResults), tsamples,
    )
    observer.armCompleted(result).as(result)
