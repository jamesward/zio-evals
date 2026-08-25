package com.jamesward.zio_evals

import zio.*
import zio.test.*

object EvalRunnerSpec extends ZIOSpecDefault:

  private val spec0 = EvalSpec(
    task = "find the answer",
    criteria = "must be correct",
    checks = List(EvalCheck.ToolCalled("search"), EvalCheck.AnswerContains("tool")),
  )

  private val arms = List(
    EvalArm.modelOnly(),
    EvalArm.mcp("toolbook", "Agent with ToolBook", List(McpServerConfig("toolbook", "http://h/x"))),
  )

  def spec = suite("EvalRunner")(
    test("runs every arm, judges all arms, aggregates + fires the observer") {
      for
        completed <- Ref.make(0)
        observer   = new EvalObserver:
                       override def armCompleted(result: ArmResult): UIO[Unit] = completed.update(_ + 1)
        judge      = AgentLoopJudge(FakeAgentLoop(), "judge")
        results   <- EvalRunner.run(spec0, arms, List("m"), samples = 2, FakeAgentLoop(), judge, observer)
        count     <- completed.get
      yield
        val modelOnly = results.find(_.arm.name == "model").get
        val tb        = results.find(_.arm.name == "toolbook").get
        assertTrue(
          results.length == 2,
          count == 2,
          // The fake judge passes every arm.
          modelOnly.verdict == EvalVerdict.Pass,
          modelOnly.passRate == 1.0,
          // model-only never called the tool -> ToolCalled check fails.
          !modelOnly.checksPassed,
          // toolbook arm called the tool and mentions it -> both checks pass.
          tb.checksPassed,
          tb.metrics.toolCalls == 1.0,
          tb.samples.length == 2,
          tb.samples.forall(_.verdict == EvalVerdict.Pass),
        )
    },
    test("aggregated transcript round-trips through the codec") {
      for
        results <- EvalRunner.run(spec0, arms, List("m"), 1, FakeAgentLoop(), AgentLoopJudge(FakeAgentLoop(), "j"))
        tb       = results.find(_.arm.name == "toolbook").get
        json     = EvalCodecs.encode(EvalTranscript.of(tb.samples))
        back     = EvalCodecs.decode[EvalTranscript](json)
      yield assertTrue(back.isRight, back.toOption.get.samples.length == 1)
    },
    test("a failing agent surfaces as an Error sample and a non-passing arm") {
      for
        results <- EvalRunner.run(spec0, List(EvalArm.modelOnly()), List("m"), 1, FailingAgentLoop(), AgentLoopJudge(FakeAgentLoop(), "j"))
      yield
        val r = results.head
        assertTrue(
          r.passRate == 0.0,
          r.verdict != EvalVerdict.Pass,
          r.samples.head.verdict == EvalVerdict.Error,
          r.samples.head.rationale.contains("boom"),
        )
    },
  )
