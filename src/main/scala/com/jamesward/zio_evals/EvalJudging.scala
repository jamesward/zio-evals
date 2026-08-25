package com.jamesward.zio_evals

import zio.json.*
import zio.json.ast.Json

// Shared "judge" logic used by every judge backend. ONE judge call grades all N
// candidate answers (labeled Arm 1..N) against the eval's rubric, using the
// judge's own MCP tools to verify facts, and returns a (verdict, rationale) per
// arm in input order. Kept backend-agnostic so a CLI judge and a hosted judge
// grade identically.
object EvalJudging:

  // The grading system prompt. Kept identical across backends.
  val judgeSystem: String =
    "You are a strict evaluation judge. Use the available tools to determine the correct answer and " +
      "verify each candidate's claims before grading — do not rely on prior knowledge when a tool can confirm the facts. " +
      "Follow the output format exactly."

  // All candidate answers labeled Arm 1..n, graded together against the rubric.
  // `answers` is (arm label, answer) in the order the arms should be graded.
  def judgePrompt(spec: EvalSpec, answers: List[(String, String)]): String =
    val n = answers.size
    val candidates = answers.zipWithIndex.map { case ((label, ans), i) => s"\n--- Arm ${i + 1} ($label) ---\n$ans" }.mkString
    s"""You are grading $n candidate answers to the same task against a rubric. The answers are labeled Arm 1..Arm $n below.
       |Use the available tools to determine the correct answer and verify each candidate — check the facts with the tools rather than assuming.
       |Grade EVERY arm (1..$n) as PASS or FAIL with a one-sentence rationale, putting each arm's number in its `arm` field.
       |
       |Task given to the assistants: ${spec.task}
       |Grading rubric: ${spec.criteria}
       |$candidates
       |
       |Respond with ONLY a JSON object of exactly this shape (no other prose), grading all $n arms:
       |{"grades":[{"arm":1,"verdict":"PASS"|"FAIL","rationale":"one sentence"}, ... one entry per arm 1..$n]}""".stripMargin

  // The judge's structured output: one grade per arm (1-based `arm` index).
  private final case class JudgeGrade(arm: Int, verdict: String, rationale: String) derives JsonDecoder
  private final case class JudgeGrades(grades: List[JudgeGrade]) derives JsonDecoder

  // JSON schema for the judge's output, for backends that can constrain output
  // (a CLI `--json-schema`). NOTE: no `minItems`/`maxItems` — some providers'
  // structured outputs reject array bounds other than 0/1; completeness is
  // enforced by the prompt + the retry (`incomplete`/`merge`) instead.
  val judgeSchema: Json =
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        "grades" -> Json.Obj(
          "type" -> Json.Str("array"),
          "items" -> Json.Obj(
            "type" -> Json.Str("object"),
            "properties" -> Json.Obj(
              "arm"       -> Json.Obj("type" -> Json.Str("integer"), "description" -> Json.Str("1-based index of the arm being graded")),
              "verdict"   -> Json.Obj("type" -> Json.Str("string"), "enum" -> Json.Arr(Json.Str("PASS"), Json.Str("FAIL"))),
              "rationale" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("one sentence")),
            ),
            "required" -> Json.Arr(Json.Str("arm"), Json.Str("verdict"), Json.Str("rationale")),
            "additionalProperties" -> Json.Bool(false),
          ),
        ),
      ),
      "required" -> Json.Arr(Json.Str("grades")),
      "additionalProperties" -> Json.Bool(false),
    )

  // Lenient JSON extraction (first `{`..last `}`): structured output is clean
  // JSON, but a chatty CLI/agent may wrap it in prose.
  def sliceJson(s: String): String =
    val i = s.indexOf('{')
    val j = s.lastIndexOf('}')
    if i >= 0 && j > i then s.substring(i, j + 1) else s

  // Parse the judge's JSON (`{ grades: [...] }`) into a (verdict, rationale) per
  // arm, in order 1..n. An arm with no grade -> Error (surfaced, not defaulted).
  def parseJudge(text: String, n: Int): List[(EvalVerdict, String)] =
    val byArm = sliceJson(text).fromJson[JudgeGrades].toOption.map(_.grades.map(g => g.arm -> g).toMap).getOrElse(Map.empty)
    (1 to n).toList.map { i =>
      byArm.get(i) match
        case Some(g) =>
          val v =
            if g.verdict.equalsIgnoreCase("PASS") then EvalVerdict.Pass
            else if g.verdict.equalsIgnoreCase("FAIL") then EvalVerdict.Fail
            else EvalVerdict.Error
          (v, if g.rationale.nonEmpty then g.rationale else g.verdict)
        case None => (EvalVerdict.Error, "judge returned no grade for this arm")
    }

  // True if any arm came back ungraded (parsed as Error) — the retry trigger.
  def incomplete(gs: List[(EvalVerdict, String)]): Boolean = gs.exists(_._1 == EvalVerdict.Error)

  // Keep each arm's real grade, filling any still-ungraded arm from the retry.
  def merge(first: List[(EvalVerdict, String)], retry: List[(EvalVerdict, String)]): List[(EvalVerdict, String)] =
    first.zip(retry).map { case (a, b) => if a._1 != EvalVerdict.Error then a else b }
