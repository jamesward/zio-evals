package com.jamesward.zio_evals

import com.jamesward.zio_typesafe_ai.TypeSafeAI
import com.jamesward.zio_typesafe_ai.orchestration.*
import com.jamesward.zio_typesafe_ai.orchestration.given
import com.jamesward.zio_typesafe_ai.orchestration.model.*
import com.jamesward.zio_typesafe_ai.orchestration.runtime.*
import zio.json.ast.Json
import zio.test.*

object OrchestrationEvalSpec extends ZIOSpecDefault:
  private val itemSchema = Json.Obj(
    "type" -> Json.Str("object"),
    "properties" -> Json.Obj("name" -> Json.Obj("type" -> Json.Str("string"))),
  )

  private val workflow = Workflow(
    Vector(
      Step.Call("list", "list_symbols", Expr.Obj(Vector.empty)),
      Step.Construct("criterion", Expr.Literal(Json.Obj())),
      Step.Filter(
        "filtered",
        Expr.Literal(Json.Arr(Json.Obj("name" -> Json.Str("Type")))),
        itemSchema,
        "find relevant types",
        Expr.Ref("criterion"),
      ),
      Step.FanOut(
        "details",
        Expr.Ref("filtered"),
        "get_symbol",
        Expr.Obj(Vector("name" -> Expr.Item(List("name")))),
        FanOutAuthorization.ReadyFiltered,
      ),
    ),
    Expr.Literal(Json.Str("answer")),
  )

  private val baseMetrics = OrchestrationMetrics(
    mode = OrchestrationMode.Plan,
    jevLogicalTurns = 8,
    jevLogicalInputTokens = 100,
    jevLogicalOutputTokens = 10,
    jevLogicalTimeMs = 100,
    jevPhysicalAttempts = 15,
    jevPhysicalSuccesses = 15,
    jevPhysicalFailures = 0,
    jevPhysicalTimeMs = 120,
    jevFilterCalls = 7,
    jevFilterInputTokens = 20,
    jevFilterOutputTokens = 2,
    jevFilterTimeMs = 30,
    extractCalls = 2,
    extractInputTokens = 30,
    extractOutputTokens = 3,
    extractTimeMs = 40,
    filterCalls = 0,
    filterInputTokens = 40,
    filterOutputTokens = 4,
    filterTimeMs = 50,
    summaryCalls = 1,
    summaryInputTokens = 50,
    summaryOutputTokens = 5,
    summaryTimeMs = 60,
    mcpPhysicalCalls = 10,
    mcpPhysicalSuccesses = 10,
    mcpPhysicalFailures = 0,
    mcpWallTimeMs = 70,
    mcpSummedTimeMs = 80,
    recoveries = 1,
    replans = 0,
    totalTimeMs = 1234,
  )

  private def result(
    mode: OrchestrationMode = OrchestrationMode.Plan,
    currentWorkflow: Workflow = workflow,
    metrics: OrchestrationMetrics = baseMetrics,
    values: Map[String, Json] = Map("filtered" -> Json.Arr(Json.Obj("name" -> Json.Str("Type")))),
    trace: Vector[String] = Vector("checkpoint-continuation:1"),
  ): OrchestrationResult = OrchestrationResult(
    mode = mode,
    workflow = currentWorkflow,
    planningTurns = Nil,
    planningUsage = TypeSafeAI.Usage(100, 10),
    planningLatencyMs = 100,
    execution = ExecutionReport(
      Json.Str("answer"),
      values,
      ExecutionMetrics(10, 4, 80, 70, 150, 1234, recoveries = 1, replans = 0),
    ),
    internalLlm = InternalLlmMetrics(),
    finalText = "answer",
    metrics = metrics.copy(mode = mode),
    actionTrace = trace,
  )

  def spec = suite("TypeSafeAI orchestration evaluation")(
    test("Ready checks use committed filter values and work without no-plan trace events") {
      val plan = result(trace = Vector.empty)
      val missingValue = result(values = Map.empty, trace = Vector("filter-outcome:Ready:1/3:attempt=1"))
      assertTrue(
        OrchestrationCheck.evaluate(OrchestrationCheck.FilterReachedReady, plan),
        !OrchestrationCheck.evaluate(OrchestrationCheck.FilterReachedReady, missingValue),
        OrchestrationCheck.evaluate(OrchestrationCheck.NoUnsafeFanOut, plan),
        OrchestrationCheck.evaluate(OrchestrationCheck.OperationCalled("list_symbols"), plan),
        OrchestrationCheck.evaluate(OrchestrationCheck.OperationCalled("get_symbol"), plan),
      )
    },
    test("mode, backend, physical-call, recovery, and replan checks compare experiment arms") {
      val jev = result()
      val llmMetrics = baseMetrics.copy(jevFilterCalls = 0, filterCalls = 2, mcpPhysicalFailures = 1)
      val llm = result(OrchestrationMode.NoPlanLlmFilters, metrics = llmMetrics)
      assertTrue(
        OrchestrationCheck.evaluate(OrchestrationCheck.ModeIs(OrchestrationMode.Plan), jev),
        OrchestrationCheck.evaluate(OrchestrationCheck.FilterBackendOnly(OrchestrationFilterBackend.Jev), jev),
        !OrchestrationCheck.evaluate(OrchestrationCheck.FilterBackendOnly(OrchestrationFilterBackend.Llm), jev),
        OrchestrationCheck.evaluate(OrchestrationCheck.ModeIs(OrchestrationMode.NoPlanLlmFilters), llm),
        OrchestrationCheck.evaluate(OrchestrationCheck.FilterBackendOnly(OrchestrationFilterBackend.Llm), llm),
        OrchestrationCheck.evaluate(OrchestrationCheck.McpCallsAtMost(10), llm),
        OrchestrationCheck.evaluate(OrchestrationCheck.McpFailuresEqual(1), llm),
        OrchestrationCheck.evaluate(OrchestrationCheck.RecoveriesAtMost(1), llm),
        OrchestrationCheck.evaluate(OrchestrationCheck.ReplansEqual(0), llm),
      )
    },
    test("adapter uses common metrics and exposes mode, logical operations, trace, and answer") {
      val adapted = OrchestrationEvalAdapter.toAgentRunResult(result())
      assertTrue(
        adapted.answer == "answer",
        adapted.iterations == 8,
        adapted.toolCalls == 10,
        adapted.inputTokens == 240L,
        adapted.outputTokens == 24L,
        adapted.latencyMs == 1234L,
        adapted.capturedToolCalls.map(_.name) == List("list_symbols", "get_symbol"),
        adapted.events.contains(TranscriptEvent.Note("orchestration:mode:plan")),
        adapted.events.contains(TranscriptEvent.Note("orchestration:checkpoint-continuation:1")),
        adapted.events.lastOption.contains(TranscriptEvent.AgentMessage("answer")),
      )
    },
  )
