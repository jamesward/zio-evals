package com.jamesward.zio_evals
package cli

import zio.json.EncoderOps
import zio.json.ast.Json

// Parser for `claude -p --output-format stream-json --verbose` output, which is
// line-delimited JSON (one JSON object per line). Pure + unit-testable (no CLI,
// no tokens) — feed it captured stdout, assert the transcript events and the
// final-result metrics.
//
// The shapes mirror the Anthropic Messages content-block model the CLI emits:
//   {"type":"system","subtype":"init", ...}
//   {"type":"assistant","message":{"content":[ <blocks> ]}, ...}
//   {"type":"user","message":{"content":[ {"type":"tool_result", ...} ]}, ...}
//   {"type":"result","subtype":"success","result":"...","num_turns":N,
//      "duration_ms":N,"is_error":false,"usage":{"input_tokens":N,"output_tokens":N}}
// Content blocks mapped: text, thinking, tool_use (incl. server_tool_use),
// tool_result, web_search_tool_result. Unknown lines/blocks are ignored (the
// stream carries observability noise we don't model), EXCEPT a `result` line
// with `is_error` or `subtype != "success"`, surfaced as a failure.
object ClaudeStreamJson:

  // The final-turn metrics extracted from the `result` line.
  final case class FinalResult(
      result:           String,
      numTurns:         Int,
      durationMs:       Long,
      inputTokens:      Long,
      outputTokens:     Long,
      totalCostUsd:     Double,
      isError:          Boolean,
      errorDetail:      Option[String],
      structuredOutput: Option[Json], // the `--json-schema` validated output, when requested
  )

  // Parsed stdout: the ordered transcript events plus the final result. The
  // `result` line is required (it's how the CLI signals completion); its
  // absence is itself an error the caller surfaces.
  final case class Parsed(events: List[TranscriptEvent], finalResult: Option[FinalResult])

  private def str(o: Json.Obj, k: String): Option[String] =
    o.get(k).flatMap(_.asString)

  private def num(o: Json.Obj, k: String): Option[Long] =
    o.get(k).flatMap(_.asNumber).map(_.value.longValue)

  private def dbl(o: Json.Obj, k: String): Option[Double] =
    o.get(k).flatMap(_.asNumber).map(_.value.doubleValue)

  // tool_result `content` is either a plain string or an array of blocks
  // (each typically a `{"type":"text","text":...}` or a structured reference).
  // Flatten to readable text either way.
  private def renderContent(j: Json): String =
    j.asString match
      case Some(s) => s
      case None =>
        j.asArray match
          case Some(blocks) =>
            blocks.toList.flatMap { b =>
              b.asObject.flatMap(o => str(o, "text")).orElse(Some(b.toJson))
            }.mkString("\n")
          case None => j.toJson

  private def blockEvents(content: Json): List[TranscriptEvent] =
    content.asArray match
      case None => content.asString.toList.map(TranscriptEvent.AgentMessage.apply)
      case Some(blocks) =>
        blocks.toList.flatMap { b =>
          b.asObject match
            case None => Nil
            case Some(o) =>
              str(o, "type") match
                case Some("text") =>
                  str(o, "text").filter(_.nonEmpty).map(TranscriptEvent.AgentMessage.apply).toList
                case Some("thinking") =>
                  str(o, "thinking").filter(_.nonEmpty).map(TranscriptEvent.Thinking.apply).toList
                case Some("redacted_thinking") =>
                  List(TranscriptEvent.Thinking("(redacted)"))
                case Some("tool_use") | Some("server_tool_use") | Some("mcp_tool_use") =>
                  val name  = str(o, "name").getOrElse("(tool)")
                  val input = o.get("input").map(_.toJson).getOrElse("{}")
                  List(TranscriptEvent.ToolCall(name, input))
                case Some("tool_result") | Some("web_search_tool_result") =>
                  val text    = o.get("content").map(renderContent).getOrElse("")
                  val isError = o.get("is_error").flatMap(_.asBoolean).getOrElse(false)
                  // The CLI's tool_result block doesn't echo the tool name; the
                  // preceding ToolCall already names it, so leave it generic.
                  List(TranscriptEvent.ToolResult("(result)", text, isError))
                case _ => Nil
        }

  private def lineEvents(o: Json.Obj): List[TranscriptEvent] =
    str(o, "type") match
      case Some("assistant") | Some("user") =>
        o.get("message").flatMap(_.asObject).flatMap(_.get("content"))
          .map(blockEvents).getOrElse(Nil)
      case _ => Nil

  private def parseFinal(o: Json.Obj): FinalResult =
    val usage   = o.get("usage").flatMap(_.asObject)
    def u(k: String): Long = usage.flatMap(usg => num(usg, k)).getOrElse(0L)
    // The result-line `usage.input_tokens` is only the UNCACHED delta; the bulk
    // of the input the model processed is in the cache-read / cache-creation
    // counters. Sum all three for real input-token throughput. `output_tokens`
    // is already the run aggregate.
    val inTok   = u("input_tokens") + u("cache_read_input_tokens") + u("cache_creation_input_tokens")
    val outTok  = u("output_tokens")
    val isError = o.get("is_error").flatMap(_.asBoolean).getOrElse(false)
    val subtype = str(o, "subtype")
    FinalResult(
      result       = str(o, "result").getOrElse(""),
      numTurns     = num(o, "num_turns").map(_.toInt).getOrElse(0),
      durationMs   = num(o, "duration_ms").getOrElse(0L),
      inputTokens  = inTok,
      outputTokens = outTok,
      totalCostUsd = dbl(o, "total_cost_usd").getOrElse(0.0),
      isError      = isError || subtype.exists(_ != "success"),
      errorDetail  = if isError then str(o, "result").orElse(str(o, "subtype")) else None,
      structuredOutput = o.get("structured_output").collect { case obj: Json.Obj => obj },
    )

  // Parse the full JSONL stdout. Lines that aren't valid JSON objects are
  // skipped (the CLI occasionally interleaves non-JSON on stdout under load);
  // the `result` line is the authoritative completion signal.
  def parse(stdout: String): Parsed =
    val objs =
      stdout.linesIterator
        .map(_.trim)
        .filter(_.nonEmpty)
        .flatMap(l => l.fromJsonObj)
        .toList
    val events = objs.flatMap(lineEvents)
    val finalR = objs.reverse.collectFirst {
      case o if str(o, "type").contains("result") => parseFinal(o)
    }
    Parsed(events, finalR)

  extension (s: String)
    private def fromJsonObj: Option[Json.Obj] =
      Json.decoder.decodeJson(s).toOption.flatMap(_.asObject)
