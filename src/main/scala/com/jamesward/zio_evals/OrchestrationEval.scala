package com.jamesward.zio_evals

import com.jamesward.zio_typesafe_ai.orchestration.*
import com.jamesward.zio_typesafe_ai.orchestration.given
import com.jamesward.zio_typesafe_ai.orchestration.model.*
import zio.*

/** Provider-neutral seam for evaluating a configured TypeSafeAI orchestration arm. */
trait OrchestrationLoop:
  def run(prompt: String, mode: OrchestrationMode): Task[OrchestrationResult]

object OrchestrationLoop:
  def run(prompt: String, mode: OrchestrationMode): RIO[OrchestrationLoop, OrchestrationResult] =
    ZIO.serviceWithZIO[OrchestrationLoop](_.run(prompt, mode))

/** Filter implementation actually observed in orchestration metrics. */
enum OrchestrationFilterBackend derives CanEqual:
  case Jev, Llm

/** Deterministic assertions over what an orchestration run actually committed. */
enum OrchestrationCheck derives CanEqual:
  case ModeIs(expected: OrchestrationMode)
  case OperationCalled(name: String)
  case OperationNotCalled(name: String)
  case FilterReachedReady
  case FilterBackendOnly(expected: OrchestrationFilterBackend)
  case NoUnsafeFanOut
  case McpCallsAtMost(maximum: Int)
  case McpFailuresEqual(expected: Int)
  case ReplansEqual(expected: Int)
  case RecoveriesAtMost(maximum: Int)

object OrchestrationCheck:
  def evaluate(check: OrchestrationCheck, result: OrchestrationResult): Boolean =
    val operations = result.workflow.steps.collect:
      case Step.Call(_, operation, _)             => operation
      case Step.FanOut(_, _, operation, _, _)     => operation
    val readyFilterIds = result.workflow.steps.collect:
      case Step.Filter(id, _, _, _, _) => id
    check match
      case OrchestrationCheck.ModeIs(expected) => result.mode == expected
      case OrchestrationCheck.OperationCalled(name)    => operations.contains(name)
      case OrchestrationCheck.OperationNotCalled(name) => !operations.contains(name)
      case OrchestrationCheck.FilterReachedReady =>
        readyFilterIds.exists(id => result.execution.values.get(id).flatMap(_.asArray).exists(_.nonEmpty))
      case OrchestrationCheck.FilterBackendOnly(expected) => expected match
        case OrchestrationFilterBackend.Jev =>
          result.metrics.jevFilterCalls > 0 && result.metrics.filterCalls == 0
        case OrchestrationFilterBackend.Llm =>
          result.metrics.filterCalls > 0 && result.metrics.jevFilterCalls == 0
      case OrchestrationCheck.NoUnsafeFanOut =>
        !result.workflow.steps.exists:
          case Step.FanOut(_, _, _, _, FanOutAuthorization.Unsafe) => true
          case _                                                    => false
      case OrchestrationCheck.McpCallsAtMost(maximum) => result.metrics.mcpPhysicalCalls <= maximum
      case OrchestrationCheck.McpFailuresEqual(expected) => result.metrics.mcpPhysicalFailures == expected
      case OrchestrationCheck.ReplansEqual(expected) => result.metrics.replans == expected
      case OrchestrationCheck.RecoveriesAtMost(maximum) => result.metrics.recoveries <= maximum

  def evaluateAll(checks: List[OrchestrationCheck], result: OrchestrationResult): Boolean =
    checks.forall(evaluate(_, result))

object OrchestrationEvalAdapter:
  /** Normalize an orchestration result into the existing eval runner result shape. */
  def toAgentRunResult(result: OrchestrationResult): AgentRunResult =
    val toolEvents = result.workflow.steps.toList.flatMap:
      case Step.Call(_, operation, _) => List(TranscriptEvent.ToolCall(operation, "{}"))
      case Step.FanOut(_, _, operation, _, authorization) =>
        List(TranscriptEvent.ToolCall(operation, s"{\"mode\":\"fanout\",\"authorization\":\"$authorization\"}"))
      case _ => Nil
    val traceEvents = TranscriptEvent.Note(s"orchestration:mode:${result.mode.wireName}") ::
      result.actionTrace.toList.map(event => TranscriptEvent.Note(s"orchestration:$event"))
    AgentRunResult(
      answer = result.finalText,
      iterations = result.metrics.jevLogicalTurns,
      toolCalls = result.metrics.mcpPhysicalCalls,
      inputTokens = result.metrics.jevLogicalInputTokens.toLong + result.metrics.jevFilterInputTokens +
        result.metrics.extractInputTokens + result.metrics.filterInputTokens + result.metrics.summaryInputTokens,
      outputTokens = result.metrics.jevLogicalOutputTokens.toLong + result.metrics.jevFilterOutputTokens +
        result.metrics.extractOutputTokens + result.metrics.filterOutputTokens + result.metrics.summaryOutputTokens,
      latencyMs = result.metrics.totalTimeMs,
      events = toolEvents ++ traceEvents :+ TranscriptEvent.AgentMessage(result.finalText),
    )
