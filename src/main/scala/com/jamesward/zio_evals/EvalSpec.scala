package com.jamesward.zio_evals

import zio.schema.*

// A deterministic, model-free assertion about a run. Transcript/answer checks
// (`ToolCalled`/`ResourceRead`/`AnswerContains`/`AnswerMatches`) run in-process
// against the `AgentRunResult`; the `Command*`/`FileExists` checks require an
// execution `Sandbox` and only apply to action-based evals. See `Checks`.
enum EvalCheck derives CanEqual, Schema:
  case ToolCalled(name: String)
  case ResourceRead(uriPrefix: String)
  case AnswerContains(substring: String)
  case AnswerMatches(regex: String)
  case CommandSucceeds(command: String)
  case CommandOutputMatches(command: String, regex: String)
  case FileExists(path: String)

// The model-facing definition of a single eval, independent of any host's
// persistence. A host maps its own stored eval entity onto this to run it.
//
//   * `task`     — the prompt handed to the agent (every arm gets the same one).
//   * `criteria` — the rubric the judge scores each arm's answer against.
//   * `checks`   — deterministic assertions evaluated alongside the judge.
final case class EvalSpec(
    task:     String,
    criteria: String,
    checks:   List[EvalCheck] = Nil,
) derives Schema

object EvalSpec:
  given CanEqual[EvalSpec, EvalSpec] = CanEqual.derived
