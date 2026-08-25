package com.jamesward.zio_evals
package cli

import zio.test.*

object ClaudeStreamJsonSpec extends ZIOSpecDefault:

  private val sample =
    """{"type":"system","subtype":"init","session_id":"abc"}
      |{"type":"assistant","message":{"content":[{"type":"thinking","thinking":"let me check"},{"type":"tool_use","name":"mcp__toolbook__search","input":{"q":"x"}}]}}
      |{"type":"user","message":{"content":[{"type":"tool_result","content":"result text","is_error":false}]}}
      |{"type":"assistant","message":{"content":[{"type":"text","text":"the answer"}]}}
      |{"type":"result","subtype":"success","result":"the answer","num_turns":3,"duration_ms":1200,"is_error":false,"usage":{"input_tokens":5,"cache_read_input_tokens":100,"cache_creation_input_tokens":20,"output_tokens":42},"total_cost_usd":0.01}""".stripMargin

  def spec = suite("ClaudeStreamJson")(
    test("parses transcript events in order") {
      val parsed = ClaudeStreamJson.parse(sample)
      assertTrue(
        parsed.events.length == 4,
        parsed.events.head == TranscriptEvent.Thinking("let me check"),
        parsed.events(1) == TranscriptEvent.ToolCall("mcp__toolbook__search", """{"q":"x"}"""),
        parsed.events(3) == TranscriptEvent.AgentMessage("the answer"),
      )
    },
    test("parses the final result with summed input tokens") {
      val fr = ClaudeStreamJson.parse(sample).finalResult.get
      assertTrue(
        fr.result == "the answer",
        fr.numTurns == 3,
        fr.durationMs == 1200L,
        fr.inputTokens == 125L, // 5 + 100 + 20
        fr.outputTokens == 42L,
        !fr.isError,
      )
    },
    test("flags an error result subtype") {
      val err = """{"type":"result","subtype":"error_max_turns","result":"nope","is_error":true}"""
      val fr = ClaudeStreamJson.parse(err).finalResult.get
      assertTrue(fr.isError, fr.errorDetail.isDefined)
    },
    test("no result line yields no finalResult") {
      assertTrue(ClaudeStreamJson.parse("""{"type":"system","subtype":"init"}""").finalResult.isEmpty)
    },
  )
