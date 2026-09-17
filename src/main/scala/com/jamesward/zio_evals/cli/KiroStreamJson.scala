package com.jamesward.zio_evals
package cli

import zio.schema.*
import zio.schema.annotation.{directDynamicMapping, fieldName}

// Parses the ACP events emitted by `kiro-cli chat --output-format stream-json`.
// Tool calls are upserts keyed by `toolCallId`: the first tool event becomes a
// backend-neutral ToolCall and the first terminal update becomes its ToolResult.
object KiroStreamJson:

  private given rawJsonSchema: Schema[DynamicValue] =
    Schema.dynamicValue.annotate(directDynamicMapping())

  final case class Parsed(events: List[TranscriptEvent], finalText: String, durationMs: Option[Long])

  private final case class WireKiroMeta(
      toolName: Option[String]
  ) derives Schema

  private final case class WireMeta(
      kiro: Option[WireKiroMeta]
  ) derives Schema

  private final case class WireUpdate(
      sessionUpdate: String,
      toolCallId:    Option[String],
      name:          Option[String],
      title:         Option[String],
      status:        Option[String],
      rawInput:      Option[DynamicValue],
      rawOutput:     Option[DynamicValue],
      content:       Option[DynamicValue],
      @fieldName("_meta") meta: Option[WireMeta],
  ) derives Schema

  private final case class WireData(
      finalText:      Option[String],
      status:         Option[String],
      message:        Option[String],
      update:         Option[WireUpdate],
      turnDurationMs: Option[Long],
  ) derives Schema

  private final case class WireLine(
      @fieldName("type") kind: String,
      data: WireData,
  ) derives Schema

  private final case class ToolState(
      name:      Option[String] = None,
      rawInput:  Option[DynamicValue] = None,
      rawOutput: Option[DynamicValue] = None,
      content:   Option[DynamicValue] = None,
  ):
    def merge(update: WireUpdate): ToolState =
      ToolState(
        update.meta.flatMap(_.kiro).flatMap(_.toolName).orElse(update.name).orElse(name),
        update.rawInput.orElse(rawInput),
        update.rawOutput.orElse(rawOutput),
        update.content.orElse(content),
      )

  private def rawJson(value: DynamicValue): String =
    EvalCodecs.encode(value)

  private def renderContent(value: DynamicValue): String =
    value match
      case DynamicValue.Primitive(text: String, _) => text
      case DynamicValue.Sequence(values)           => values.toList.map(renderContent).filter(_.nonEmpty).mkString("\n")
      case record @ DynamicValue.Record(_, fields) =>
        fields.collectFirst {
          case ("text", DynamicValue.Primitive(text: String, _)) => text
          case ("content", nested)                               => renderContent(nested)
        }.getOrElse(rawJson(record))
      case other => rawJson(other)

  private def coalesceChunks(events: List[TranscriptEvent]): List[TranscriptEvent] =
    events.foldRight(List.empty[TranscriptEvent]) {
      case (TranscriptEvent.AgentMessage(left), TranscriptEvent.AgentMessage(right) :: tail) =>
        TranscriptEvent.AgentMessage(left + right) :: tail
      case (TranscriptEvent.Thinking(left), TranscriptEvent.Thinking(right) :: tail) =>
        TranscriptEvent.Thinking(left + right) :: tail
      case (event, tail) => event :: tail
    }

  private def streamEvents(updates: List[WireUpdate]): List[TranscriptEvent] =
    val identifiedTools = updates.zipWithIndex.collect {
      case (update, index) if update.sessionUpdate == "tool_call" || update.sessionUpdate == "tool_call_update" =>
        update.toolCallId.getOrElse(s"(tool-$index)") -> update
    }
    val finalStates = identifiedTools.foldLeft(Map.empty[String, ToolState]) { case (states, (id, update)) =>
      states.updated(id, states.getOrElse(id, ToolState()).merge(update))
    }

    val (_, _, _, events) = updates.zipWithIndex.foldLeft(
      (Set.empty[String], Set.empty[String], 0, List.empty[TranscriptEvent])
    ) {
      case ((called, completed, anonymousIndex, events), (update, index)) =>
        update.sessionUpdate match
          case "agent_message_chunk" =>
            val chunk = update.content.map(renderContent).filter(_.nonEmpty).map(TranscriptEvent.AgentMessage.apply).toList
            (called, completed, anonymousIndex, events ++ chunk)
          case "agent_thought_chunk" =>
            val chunk = update.content.map(renderContent).filter(_.nonEmpty).map(TranscriptEvent.Thinking.apply).toList
            (called, completed, anonymousIndex, events ++ chunk)
          case "tool_call" | "tool_call_update" =>
            val id    = update.toolCallId.getOrElse(s"(tool-$index)")
            val state = finalStates(id)
            val name  = state.name.getOrElse("(tool)")
            val callEvents =
              if called(id) then Nil
              else List(TranscriptEvent.ToolCall(name, state.rawInput.map(rawJson).getOrElse("{}")))
            val terminal = update.status.exists(s => s == "completed" || s == "failed")
            val resultEvents =
              if !terminal || completed(id) then Nil
              else
                val text = state.rawOutput.orElse(state.content).map(renderContent).getOrElse("")
                List(TranscriptEvent.ToolResult(name, text, update.status.contains("failed")))
            (called + id, if terminal then completed + id else completed, anonymousIndex + 1, events ++ callEvents ++ resultEvents)
          case _ => (called, completed, anonymousIndex, events)
    }
    coalesceChunks(events)

  def parse(stdout: String): Either[String, Parsed] =
    val lines = stdout.linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(line => EvalCodecs.decode[WireLine](line).toOption)
      .toList

    val terminal = lines.reverse.collectFirst {
      case line if line.kind == "runFinished" && line.data.status.contains("success") =>
        line.data.finalText.filter(_.nonEmpty).toRight("kiro-cli runFinished contained no finalText")
      case line if line.kind == "runError" =>
        Left(line.data.message.getOrElse("kiro-cli reported runError"))
    }.getOrElse(Left("kiro-cli stream-json contained no runFinished/runError event"))

    terminal.map { answer =>
      val streamed = streamEvents(lines.flatMap(_.data.update))
      val streamedAnswer = streamed.collect { case TranscriptEvent.AgentMessage(text) => text }.mkString
      val events = if streamedAnswer == answer then streamed else streamed :+ TranscriptEvent.AgentMessage(answer)
      val durationMs = lines.reverseIterator.flatMap(_.data.turnDurationMs).nextOption()
      Parsed(events, answer, durationMs)
    }

  // Compatibility helper for callers that only need the terminal answer.
  def finalText(stdout: String): Either[String, String] =
    parse(stdout).map(_.finalText)
