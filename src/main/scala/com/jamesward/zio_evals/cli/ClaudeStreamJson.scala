package com.jamesward.zio_evals
package cli

import zio.json.EncoderOps
import zio.json.ast.Json
import zio.schema.*
import zio.schema.annotation.{directDynamicMapping, fieldName}

// Typed parser for `claude -p --output-format stream-json --verbose` JSONL.
// Known protocol envelopes/blocks are Schema-derived records; arbitrary tool
// payloads/results use Schema `DynamicValue` with direct JSON mapping. `Json`
// remains only at the public structured-output boundary required by AgentLoop.
object ClaudeStreamJson:

  // Arbitrary tool payload/result/structured-output JSON, encoded and decoded
  // as natural JSON instead of the `DynamicValue` ADT representation.
  private given rawJsonSchema: Schema[DynamicValue] =
    Schema.dynamicValue.annotate(directDynamicMapping())

  final case class FinalResult(
      result:           String,
      numTurns:         Int,
      durationMs:       Long,
      inputTokens:      Long,
      outputTokens:     Long,
      totalCostUsd:     Double,
      isError:          Boolean,
      errorDetail:      Option[String],
      structuredOutput: Option[Json],
  )

  final case class Parsed(events: List[TranscriptEvent], finalResult: Option[FinalResult])

  private final case class WireUsage(
      @fieldName("input_tokens") inputTokens:                         Option[Long],
      @fieldName("cache_read_input_tokens") cacheReadInputTokens:     Option[Long],
      @fieldName("cache_creation_input_tokens") cacheCreatedTokens:   Option[Long],
      @fieldName("output_tokens") outputTokens:                       Option[Long],
  ) derives Schema

  private final case class WireBlock(
      @fieldName("type") kind:       String,
      text:                          Option[String],
      thinking:                      Option[String],
      name:                          Option[String],
      input:                         Option[DynamicValue],
      content:                       Option[DynamicValue],
      @fieldName("is_error") isError: Option[Boolean],
  ) derives Schema

  private final case class WireMessage(
      content: List[WireBlock]
  ) derives Schema

  private final case class WireLine(
      @fieldName("type") kind:                         String,
      subtype:                                         Option[String],
      message:                                         Option[WireMessage],
      result:                                          Option[String],
      @fieldName("num_turns") numTurns:                Option[Int],
      @fieldName("duration_ms") durationMs:             Option[Long],
      @fieldName("is_error") isError:                  Option[Boolean],
      usage:                                            Option[WireUsage],
      @fieldName("total_cost_usd") totalCostUsd:        Option[Double],
      @fieldName("structured_output") structuredOutput: Option[DynamicValue],
  ) derives Schema

  private def rawJson(value: DynamicValue): String =
    EvalCodecs.encode(value)

  private def renderContent(value: DynamicValue): String =
    value match
      case DynamicValue.Primitive(text: String, _) => text
      case DynamicValue.Sequence(values) =>
        values.toList.map {
          case record @ DynamicValue.Record(_, fields) =>
            fields.collectFirst {
              case ("text", DynamicValue.Primitive(text: String, _)) => text
            }.getOrElse(rawJson(record))
          case other => rawJson(other)
        }.mkString("\n")
      case other => rawJson(other)

  private def blockEvent(block: WireBlock): List[TranscriptEvent] =
    block.kind match
      case "text" =>
        block.text.filter(_.nonEmpty).map(TranscriptEvent.AgentMessage.apply).toList
      case "thinking" =>
        block.thinking.filter(_.nonEmpty).map(TranscriptEvent.Thinking.apply).toList
      case "redacted_thinking" =>
        List(TranscriptEvent.Thinking("(redacted)"))
      case "tool_use" | "server_tool_use" | "mcp_tool_use" =>
        List(TranscriptEvent.ToolCall(block.name.getOrElse("(tool)"), block.input.map(rawJson).getOrElse("{}")))
      case "tool_result" | "web_search_tool_result" =>
        List(TranscriptEvent.ToolResult("(result)", block.content.map(renderContent).getOrElse(""), block.isError.contains(true)))
      case _ => Nil

  private def lineEvents(line: WireLine): List[TranscriptEvent] =
    line.kind match
      case "assistant" | "user" => line.message.toList.flatMap(_.content.flatMap(blockEvent))
      case _                      => Nil

  private def finalResult(line: WireLine): FinalResult =
    val usage = line.usage
    val input = usage.flatMap(_.inputTokens).getOrElse(0L) +
      usage.flatMap(_.cacheReadInputTokens).getOrElse(0L) +
      usage.flatMap(_.cacheCreatedTokens).getOrElse(0L)
    val rawError = line.isError.contains(true)
    val failed   = rawError || line.subtype.exists(_ != "success")
    FinalResult(
      result           = line.result.getOrElse(""),
      numTurns         = line.numTurns.getOrElse(0),
      durationMs       = line.durationMs.getOrElse(0L),
      inputTokens      = input,
      outputTokens     = usage.flatMap(_.outputTokens).getOrElse(0L),
      totalCostUsd     = line.totalCostUsd.getOrElse(0.0),
      isError          = failed,
      errorDetail      = if failed then line.result.orElse(line.subtype) else None,
      structuredOutput = line.structuredOutput
                           .flatMap(value => Json.decoder.decodeJson(rawJson(value)).toOption)
                           .collect { case obj: Json.Obj => obj },
    )

  def parse(stdout: String): Parsed =
    val lines = stdout.linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(line => EvalCodecs.decode[WireLine](line).toOption)
      .toList
    Parsed(
      events      = lines.flatMap(lineEvents),
      finalResult = lines.reverse.collectFirst { case line if line.kind == "result" => finalResult(line) },
    )
