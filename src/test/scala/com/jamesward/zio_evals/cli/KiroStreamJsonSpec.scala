package com.jamesward.zio_evals
package cli

import zio.test.*

object KiroStreamJsonSpec extends ZIOSpecDefault:

  def spec = suite("KiroStreamJson")(
    test("extracts lossless final text from runFinished") {
      val stdout =
        """{"type":"runStarted","data":{"payloadSchema":"acp","engine":"v2"}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"ignored"}}}}
          |{"type":"runFinished","data":{"status":"success","finalText":"case _ => left *> right","finalTextTruncated":false}}
          |""".stripMargin
      assertTrue(KiroStreamJson.finalText(stdout) == Right("case _ => left *> right"))
    },
    test("captures and coalesces intermediate thought and message chunks") {
      val stdout =
        """{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"let "}}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"me think"}}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"the "}}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"answer"}}}}
          |{"type":"runFinished","data":{"status":"success","finalText":"the answer"}}
          |""".stripMargin
      val parsed = KiroStreamJson.parse(stdout).toOption.get
      assertTrue(
        parsed.events == List(
          TranscriptEvent.Thinking("let me think"),
          TranscriptEvent.AgentMessage("the answer"),
        )
      )
    },
    test("captures ACP tool calls and correlates terminal results") {
      val stdout =
        """{"type":"runStarted","data":{"payloadSchema":"acp","engine":"v2"}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"tool_call","toolCallId":"call-1","title":"Searching","kind":"search","status":"pending","rawInput":{"q":"zio"},"_meta":{"kiro":{"toolName":"mcp__atlas__search"}}}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"tool_call_update","toolCallId":"call-1","status":"in_progress"}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"tool_call_update","toolCallId":"call-1","status":"completed","rawOutput":{"hits":2}}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"tool_call","toolCallId":"call-2","name":"fetch","title":"Fetching","status":"pending","rawInput":{"url":"https://example.test"}}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"tool_call_update","toolCallId":"call-2","status":"failed","content":[{"type":"content","content":{"type":"text","text":"network denied"}}]}}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"tool_call","toolCallId":"call-3","title":"Searching private data","status":"completed","rawInput":{"q":"secret"}}}}
          |{"type":"metadata","data":{"contextUsagePercentage":0.5,"turnDurationMs":321}}
          |{"type":"runFinished","data":{"status":"success","finalText":"done","finalTextTruncated":false}}
          |""".stripMargin
      val parsed = KiroStreamJson.parse(stdout).toOption.get
      assertTrue(
        parsed.finalText == "done",
        parsed.durationMs.contains(321L),
        parsed.events == List(
          TranscriptEvent.ToolCall("mcp__atlas__search", """{"q":"zio"}"""),
          TranscriptEvent.ToolResult("mcp__atlas__search", """{"hits":2}""", isError = false),
          TranscriptEvent.ToolCall("fetch", """{"url":"https://example.test"}"""),
          TranscriptEvent.ToolResult("fetch", "network denied", isError = true),
          TranscriptEvent.ToolCall("(tool)", """{"q":"secret"}"""),
          TranscriptEvent.ToolResult("(tool)", "", isError = false),
          TranscriptEvent.AgentMessage("done"),
        ),
      )
    },
    test("surfaces a runError") {
      val stdout = """{"type":"runError","data":{"stage":"engine","message":"bad engine"}}"""
      assertTrue(KiroStreamJson.finalText(stdout) == Left("bad engine"))
    },
    test("fails when the stream has no terminal event") {
      val stdout = """{"type":"metadata","data":{"sessionId":"s"}}"""
      assertTrue(KiroStreamJson.finalText(stdout).isLeft)
    },
  )
