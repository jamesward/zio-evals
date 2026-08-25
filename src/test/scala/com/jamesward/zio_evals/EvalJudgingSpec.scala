package com.jamesward.zio_evals

import zio.test.*

object EvalJudgingSpec extends ZIOSpecDefault:

  private val spec0 = EvalSpec("what is 2+2?", "answer must be 4")

  def spec = suite("EvalJudging")(
    test("judgePrompt labels every arm and includes task + rubric") {
      val p = EvalJudging.judgePrompt(spec0, List("Agent alone" -> "4", "Agent with tool" -> "four"))
      assertTrue(
        p.contains("--- Arm 1 (Agent alone) ---"),
        p.contains("--- Arm 2 (Agent with tool) ---"),
        p.contains("what is 2+2?"),
        p.contains("answer must be 4"),
      )
    },
    test("parseJudge maps verdicts by arm index") {
      val text = """prose {"grades":[{"arm":1,"verdict":"PASS","rationale":"ok"},{"arm":2,"verdict":"FAIL","rationale":"no"}]} trailing"""
      val vs = EvalJudging.parseJudge(text, 2)
      assertTrue(vs.head._1 == EvalVerdict.Pass, vs(1)._1 == EvalVerdict.Fail)
    },
    test("parseJudge marks an ungraded arm Error") {
      val vs = EvalJudging.parseJudge("""{"grades":[{"arm":1,"verdict":"PASS","rationale":"ok"}]}""", 2)
      assertTrue(vs.head._1 == EvalVerdict.Pass, vs(1)._1 == EvalVerdict.Error)
    },
    test("incomplete + merge fills the retry grade") {
      val first  = List((EvalVerdict.Pass, "ok"), (EvalVerdict.Error, "missing"))
      val retry  = List((EvalVerdict.Fail, "wrong"), (EvalVerdict.Fail, "wrong2"))
      val merged = EvalJudging.merge(first, retry)
      assertTrue(
        EvalJudging.incomplete(first),
        merged.head._1 == EvalVerdict.Pass,
        merged.head._2 == "ok",
        merged(1)._1 == EvalVerdict.Fail,
        merged(1)._2 == "wrong2",
      )
    },
    test("sliceJson extracts the JSON object from prose") {
      assertTrue(EvalJudging.sliceJson("""noise {"a":1} tail""") == """{"a":1}""")
    },
  )
