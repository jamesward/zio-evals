package com.jamesward.zio_evals

import com.jamesward.zio_typesafe_ai.TypeSafeAI
import com.jamesward.zio_typesafe_ai.TypeSafeAI.*
import com.jamesward.zio_typesafe_ai.TypeSafeAI.Error.retryOnRetryable
import zio.*
import zio.schema.*

/** A [[Judge]] backed by TypeSafe AI's Jev / System One model.
  *
  * Jev answers typed atomic questions rather than generating free-form text,
  * so this judge asks one `Noul` (binary) question per candidate. The returned
  * calibrated probability becomes `Pass` when it is greater than or equal to
  * `passThreshold`, and `Fail` otherwise. Provider failures are isolated to
  * the affected candidate and surfaced as `EvalVerdict.Error`.
  */
final class JevJudge private (
    classify:      JevJudge.Candidate => Task[JevJudge.Classification],
    passThreshold: Double,
) extends Judge:

  require(
    passThreshold >= 0.0 && passThreshold <= 1.0,
    s"passThreshold must be within [0.0, 1.0], got $passThreshold",
  )

  def judge(spec: EvalSpec, answers: List[(EvalArm, String)]): Task[JudgeOutcome] =
    ZIO.foreach(answers.zipWithIndex) { case ((arm, answer), index) =>
      val candidate = JevJudge.Candidate(spec.task, spec.criteria, answer)
      classify(candidate).fold(
        error =>
          val message = Option(error.getMessage)
            .filter(_.nonEmpty)
            .getOrElse(error.getClass.getSimpleName)
          val rationale = s"Jev judge failed: $message"
          ((EvalVerdict.Error, rationale), TranscriptEvent.Note(s"Jev arm ${index + 1} (${arm.label}): $rationale")),
        result =>
          val passed     = result.passProbability >= passThreshold
          val verdict    = if passed then EvalVerdict.Pass else EvalVerdict.Fail
          val comparison = if passed then "met" else "was below"
          val rationale = f"Jev pass probability ${result.passProbability}%.4f $comparison threshold $passThreshold%.4f."
          val note = TranscriptEvent.Note(
            s"Jev arm ${index + 1} (${arm.label}): $rationale " +
              s"model=${result.modelId}, inputTokens=${result.inputTokens}, outputTokens=${result.outputTokens}",
          )
          ((verdict, rationale), note),
      )
    }.map(results => JudgeOutcome(results.map(_._1), results.map(_._2)))

object JevJudge:

  private val question = Question.Noul(
    "Does the candidate answer fully satisfy the grading rubric for the task? Be strict: answer true only when the response is correct and meets every stated requirement.",
    NoulCriteria(
      whenTrue  = "The candidate answer is correct and fully satisfies the rubric.",
      whenFalse = "The candidate answer is incorrect, incomplete, or violates any part of the rubric.",
    ),
  )

  private[zio_evals] final case class Candidate(
      task:   String,
      rubric: String,
      answer: String,
  ) derives Schema

  private[zio_evals] final case class Classification(
      passProbability: Double,
      modelId:         String,
      inputTokens:     Int,
      outputTokens:    Int,
  )

  /** Builds a judge from an already configured TypeSafe AI client. The
    * client's model is used (normally `jev-latest`). Retryable provider errors
    * are retried according to zio-typesafe-ai's built-in backoff policy.
    */
  def apply(client: TypeSafeAI.Client, passThreshold: Double = 0.5): JevJudge =
    fromClassifier(passThreshold) { candidate =>
      TypeSafeAI
        .ask(candidate, (passesRubric = question))
        .run
        .retryOnRetryable
        .provideEnvironment(ZEnvironment(client))
        .map { result =>
          Classification(
            result.answers.passesRubric.unwrap,
            result.model.unwrap,
            result.usage.inputTokens,
            result.usage.outputTokens,
          )
        }
    }

  /** Obtains the configured TypeSafe AI client from the ZIO environment. */
  def make(passThreshold: Double = 0.5): ZIO[TypeSafeAI.Client, Nothing, JevJudge] =
    ZIO.serviceWith(client => JevJudge(client, passThreshold))

  private[zio_evals] def fromClassifier(
      passThreshold: Double,
  )(
      classify: Candidate => Task[Classification],
  ): JevJudge =
    new JevJudge(classify, passThreshold)
