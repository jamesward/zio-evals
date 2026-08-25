package com.jamesward.zio_evals

import zio.test.*

object EvalCodecsSpec extends ZIOSpecDefault:

  def spec = suite("EvalCodecs")(
    test("EvalTranscript round-trips through schema JSON") {
      val t = EvalTranscript.of(List(
        TranscriptSample(
          arm = "toolbook", modelId = "m",
          events = List(TranscriptEvent.AgentMessage("hi"), TranscriptEvent.ToolCall("t", "{}"), TranscriptEvent.Note("done")),
          answer = "ans", verdict = EvalVerdict.Pass, rationale = "ok",
        ),
      ))
      val json = EvalCodecs.encode(t)
      val back = EvalCodecs.decode[EvalTranscript](json)
      assertTrue(back.isRight, back.toOption.get == t, json.contains("toolbook"))
    },
    test("decode of corrupt json is a Left") {
      assertTrue(EvalCodecs.decode[EvalTranscript]("not json").isLeft)
    },
  )
