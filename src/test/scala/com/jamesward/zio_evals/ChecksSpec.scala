package com.jamesward.zio_evals

import zio.test.*

object ChecksSpec extends ZIOSpecDefault:

  private val res = AgentRunResult(
    answer = "the answer is 4",
    iterations = 1, toolCalls = 1, inputTokens = 0, outputTokens = 0, latencyMs = 0,
    events = List(
      TranscriptEvent.ToolCall("mcp__tb__search", """{"uri":"skill://intro"}"""),
      TranscriptEvent.ToolResult("mcp__tb__search", "body", isError = false),
      TranscriptEvent.AgentMessage("the answer is 4"),
    ),
  )

  def spec = suite("Checks")(
    test("transcript/answer checks evaluate in-process") {
      assertTrue(
        Checks.transcriptCheck(EvalCheck.ToolCalled("search"), res).contains(true),
        Checks.transcriptCheck(EvalCheck.ToolCalled("missing"), res).contains(false),
        Checks.transcriptCheck(EvalCheck.AnswerContains("4"), res).contains(true),
        Checks.transcriptCheck(EvalCheck.AnswerMatches("answer is \\d"), res).contains(true),
        Checks.transcriptCheck(EvalCheck.ResourceRead("skill://"), res).contains(true),
        Checks.transcriptCheck(EvalCheck.CommandSucceeds("ls"), res).isEmpty,
      )
    },
    test("evaluateAll: empty checks pass; transcript checks honored") {
      for
        none <- Checks.evaluateAll(Nil, res)
        ok   <- Checks.evaluateAll(List(EvalCheck.AnswerContains("4")), res)
        bad  <- Checks.evaluateAll(List(EvalCheck.AnswerContains("nope")), res)
      yield assertTrue(none, ok, !bad)
    },
    test("evaluateAll: a command check with no workspace fails honestly") {
      for pass <- Checks.evaluateAll(List(EvalCheck.CommandSucceeds("true")), res)
      yield assertTrue(!pass)
    },
  )
