package com.jamesward.zio_evals

import zio.*

import scala.util.matching.Regex
import scala.util.Try

// Evaluates the deterministic, model-free `EvalCheck`s. Two families:
//   * transcript/answer checks — pure, run against the `AgentRunResult`;
//   * command/file checks      — require a `Workspace` (from a `Sandbox`).
// Each check evaluates to `Some(pass)` when it's applicable to the family being
// run, or `None` when it isn't (a command check has no meaning without a
// workspace). `evaluateAll` combines them: with no workspace, a command/file
// check fails honestly (it can't pass unverified) rather than being skipped.
object Checks:

  // Pure transcript/answer checks. `None` for the sandbox-only checks.
  def transcriptCheck(check: EvalCheck, result: AgentRunResult): Option[Boolean] =
    check match
      case EvalCheck.ToolCalled(name) =>
        Some(result.events.exists {
          case TranscriptEvent.ToolCall(n, _) => n == name || n.endsWith(name)
          case _                              => false
        })
      case EvalCheck.ResourceRead(uriPrefix) =>
        Some(result.events.exists {
          case TranscriptEvent.ToolCall(_, input)   => input.contains(uriPrefix)
          case TranscriptEvent.ToolResult(_, t, _)  => t.contains(uriPrefix)
          case _                                    => false
        })
      case EvalCheck.AnswerContains(substring) =>
        Some(result.answer.contains(substring))
      case EvalCheck.AnswerMatches(regex) =>
        Some(compile(regex).exists(_.findFirstIn(result.answer).isDefined))
      case _ => None

  // Sandbox checks against a provisioned workspace. `None` for transcript checks.
  def sandboxCheck(check: EvalCheck, ws: Workspace, timeout: Duration): IO[SandboxError, Option[Boolean]] =
    check match
      case EvalCheck.CommandSucceeds(command) =>
        ws.run(command, timeout).map(r => Some(r.exitCode == 0))
      case EvalCheck.CommandOutputMatches(command, regex) =>
        ws.run(command, timeout).map(r => Some(compile(regex).exists(_.findFirstIn(r.stdout).isDefined)))
      case EvalCheck.FileExists(path) =>
        ws.readFile(path).as(Some(true)).catchAll(_ => ZIO.some(false))
      case _ => ZIO.none

  // Evaluate every check and AND the results. `workspace` is required for the
  // command/file checks; when absent, such a check contributes `false` (an
  // unverifiable action check must not silently pass). Transcript/answer checks
  // never need a workspace. Empty `checks` => `true` (nothing to disprove).
  def evaluateAll(
      checks:    List[EvalCheck],
      result:    AgentRunResult,
      workspace: Option[Workspace] = None,
      timeout:   Duration = 60.seconds,
  ): IO[SandboxError, Boolean] =
    ZIO.foreach(checks) { c =>
      transcriptCheck(c, result) match
        case Some(pass) => ZIO.succeed(pass)
        case None =>
          workspace match
            case Some(ws) => sandboxCheck(c, ws, timeout).map(_.getOrElse(false))
            case None     => ZIO.succeed(false)
    }.map(_.forall(identity))

  private def compile(regex: String): Option[Regex] =
    Try(regex.r).toOption
