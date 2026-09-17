package com.jamesward.zio_evals

import com.jamesward.zio_typesafe_ai.TypeSafeAI
import com.jamesward.zio_typesafe_ai.TypeSafeAI.*
import zio.*
import zio.http.{Client as HttpClient}
import zio.test.*
import zio.test.TestAspect.*

/** Live-test infrastructure provides `TYPESAFE_AI_KEY`; applications using
  * `TypeSafeAI.Client.live` should use the SDK-standard `TYPESAFE_API_KEY`.
  */
object JevJudgeIntegrationSpec extends ZIOSpecDefault:

  private def envOr(name: String, default: String): String =
    Option(java.lang.System.getenv(name)).getOrElse(default)

  private val apiKey = ApiKey(envOr("TYPESAFE_AI_KEY", ""))
  private val model  = ModelId(envOr("TYPESAFE_TEST_MODEL", ModelId.JevLatest.unwrap))

  private val jevLayer: ZLayer[HttpClient, Nothing, TypeSafeAI.Client] =
    TypeSafeAI.Client.layer(apiKey, model)

  def spec = suite("JevJudge live integration")(
    test("grades an obvious correct and incorrect answer") {
      val eval = EvalSpec(
        task     = "What is 2 + 2? Reply with one integer.",
        criteria = "The answer must be the integer 4 and must not claim any other result.",
      )
      val candidates = List(
        EvalArm.modelOnly("a", "Candidate A") -> "4",
        EvalArm.modelOnly("b", "Candidate B") -> "5",
      )

      for
        judge   <- JevJudge.make()
        outcome <- judge.judge(eval, candidates)
      yield assertTrue(
        outcome.verdicts.map(_._1) == List(EvalVerdict.Pass, EvalVerdict.Fail),
        outcome.events.size == 2,
        outcome.events.forall {
          case TranscriptEvent.Note(text) => text.contains("model=") && text.contains("inputTokens=")
          case _                          => false
        },
      )
    }
  ).provideSomeShared[Scope](HttpClient.default, jevLayer)
    @@ ifEnvSet("TYPESAFE_AI_KEY")
    @@ withLiveClock
    @@ withLiveSystem
    @@ timeout(90.seconds)
    @@ sequential
