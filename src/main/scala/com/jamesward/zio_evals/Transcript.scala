package com.jamesward.zio_evals

import zio.schema.*

// The full agent transcript for an eval run — every turn: the agent's visible
// messages, its thinking/reasoning, each tool call + the result it got back,
// and the final answer.
//
// Provider-agnostic on purpose: a CLI backend's `claude -p` stream-json events,
// a kiro-cli plain-text answer, and a hosted-agent event stream all map onto
// the same `TranscriptEvent` ADT, so read surfaces render one shape regardless
// of which backend produced it.

// The judge's (or a deterministic check's) outcome for one candidate answer.
enum EvalVerdict derives CanEqual, Schema:
  case Pass, Fail, Error

// One step in the agent's conversation. Closed ADT (exhaustive matching for the
// renderers) covering what the backends can observe.
enum TranscriptEvent derives Schema:
  // The agent's user-visible assistant text for a turn.
  case AgentMessage(text: String)
  // The agent's internal reasoning ("thinking"/"reasoning") when the backend
  // exposes it. Kept distinct from `AgentMessage` so a UI can label it.
  case Thinking(text: String)
  // A tool invocation by the agent. `input` is the raw JSON arguments text.
  case ToolCall(name: String, input: String)
  // The result handed back for a prior `ToolCall`. `isError` flags a failed
  // tool. `text` is the (possibly truncated) result text.
  case ToolResult(name: String, text: String, isError: Boolean)
  // A backend/system note (e.g. session init, an error surfaced mid-run, a
  // timeout). Never silently dropped — surfaced so failures are visible.
  case Note(text: String)

object TranscriptEvent:
  given CanEqual[TranscriptEvent, TranscriptEvent] = CanEqual.derived

// One sample's conversation: the ordered events plus the scored outcome the
// judge assigned. `arm` and `modelId` let a multi-arm/multi-model run's
// transcript be grouped on read.
final case class TranscriptSample(
    arm:       String,
    modelId:   String,
    events:    List[TranscriptEvent],
    answer:    String,
    verdict:   EvalVerdict,
    rationale: String,
) derives Schema

object TranscriptSample:
  given CanEqual[TranscriptSample, TranscriptSample] = CanEqual.derived

// The full decoded transcript body for one (arm, model) run cell: every
// sample's conversation. Version-tagged so a future shape change can be
// detected rather than mis-decoded.
final case class EvalTranscript(
    version: Int,
    samples: List[TranscriptSample],
) derives Schema

object EvalTranscript:
  val CurrentVersion: Int = 1

  def of(samples: List[TranscriptSample]): EvalTranscript =
    EvalTranscript(CurrentVersion, samples)

  given CanEqual[EvalTranscript, EvalTranscript] = CanEqual.derived
