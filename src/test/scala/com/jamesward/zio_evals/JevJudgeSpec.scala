package com.jamesward.zio_evals

import zio.*
import zio.test.*

object JevJudgeSpec extends ZIOSpecDefault:

  private val spec0 = EvalSpec("What is 2 + 2?", "The answer must be exactly 4.")
  private val arms = List(
    EvalArm.modelOnly("a", "First"),
    EvalArm.modelOnly("b", "Second"),
    EvalArm.modelOnly("c", "Third"),
  )

  private def result(probability: Double): JevJudge.Classification =
    JevJudge.Classification(probability, "jev-test", inputTokens = 12, outputTokens = 3)

  def spec = suite("JevJudge")(
    test("preserves candidate order and maps probabilities at the inclusive threshold") {
      for
        seen <- Ref.make(List.empty[JevJudge.Candidate])
        judge = JevJudge.fromClassifier(passThreshold = 0.7) { candidate =>
                  seen.update(_ :+ candidate) *>
                    ZIO.succeed(result(if candidate.answer == "high" then 0.9 else 0.7))
                }
        outcome <- judge.judge(spec0, List(arms(0) -> "high", arms(1) -> "boundary"))
        inputs  <- seen.get
      yield assertTrue(
        inputs.map(_.answer) == List("high", "boundary"),
        inputs.forall(_.task == spec0.task),
        inputs.forall(_.rubric == spec0.criteria),
        outcome.verdicts.map(_._1) == List(EvalVerdict.Pass, EvalVerdict.Pass),
        outcome.verdicts.head._2.contains("0.9000"),
        outcome.verdicts(1)._2.contains("0.7000"),
      )
    },
    test("uses each arm's task and system-prompt context for classification") {
      val variant = EvalArm.modelOnly(
        "variant",
        "Variant",
        systemPrompt = Some("Use https://example.test/variant"),
        taskOverride = Some("What is 3 + 3?"),
      )
      for
        seen <- Ref.make(Option.empty[JevJudge.Candidate])
        judge = JevJudge.fromClassifier(0.5)(candidate => seen.set(Some(candidate)).as(result(1.0)))
        _ <- judge.judge(spec0, List(variant -> "6"))
        candidate <- seen.get
      yield assertTrue(
        candidate.exists(_.task == "What is 3 + 3?"),
        candidate.flatMap(_.systemPrompt).contains("Use https://example.test/variant"),
      )
    },
    test("maps probabilities below the threshold to Fail and records provider metadata") {
      val judge = JevJudge.fromClassifier(0.5)(_ => ZIO.succeed(result(0.49)))
      for outcome <- judge.judge(spec0, List(arms.head -> "5"))
      yield
        val notes = outcome.events.collect { case TranscriptEvent.Note(text) => text }
        assertTrue(
          outcome.verdicts.map(_._1) == List(EvalVerdict.Fail),
          outcome.verdicts.head._2.contains("was below threshold"),
          notes.exists(_.contains("model=jev-test")),
          notes.exists(_.contains("inputTokens=12")),
          notes.exists(_.contains("outputTokens=3")),
        )
    },
    test("isolates a provider failure to its candidate") {
      val judge = JevJudge.fromClassifier(0.5) { candidate =>
        if candidate.answer == "error" then ZIO.fail(RuntimeException("provider unavailable"))
        else ZIO.succeed(result(if candidate.answer == "pass" then 0.8 else 0.2))
      }
      for outcome <- judge.judge(spec0, arms.zip(List("pass", "error", "fail")))
      yield assertTrue(
        outcome.verdicts.map(_._1) == List(EvalVerdict.Pass, EvalVerdict.Error, EvalVerdict.Fail),
        outcome.verdicts(1)._2.contains("provider unavailable"),
        outcome.events.length == 3,
      )
    },
    test("does not invoke Jev for an empty candidate list") {
      for
        calls <- Ref.make(0)
        judge = JevJudge.fromClassifier(0.5)(_ => calls.update(_ + 1).as(result(1.0)))
        outcome <- judge.judge(spec0, Nil)
        count   <- calls.get
      yield assertTrue(count == 0, outcome.verdicts.isEmpty, outcome.events.isEmpty)
    },
    test("propagates classifier defects instead of reporting a provider failure") {
      val judge = JevJudge.fromClassifier(0.5)(_ => ZIO.die(RuntimeException("defect")))
      for exit <- judge.judge(spec0, List(arms.head -> "4")).exit
      yield assertTrue(exit.causeOption.exists(_.defects.exists(_.getMessage == "defect")))
    },
    test("rejects a pass threshold outside the probability range") {
      assertTrue(
        scala.util.Try(JevJudge.fromClassifier(-0.1)(_ => ZIO.succeed(result(1.0)))).isFailure,
        scala.util.Try(JevJudge.fromClassifier(1.1)(_ => ZIO.succeed(result(1.0)))).isFailure,
      )
    },
  )
