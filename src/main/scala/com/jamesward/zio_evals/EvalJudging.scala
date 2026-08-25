package com.jamesward.zio_evals

import zio.http.endpoint.openapi.{JsonSchema as JS}
import zio.json.ast.Json
import zio.schema.*

// Shared judge logic used by every backend. ONE judge call grades all candidate
// answers against the rubric and returns a verdict/rationale per arm.
object EvalJudging:

  val judgeSystem: String =
    "You are a strict evaluation judge. Use the available tools to determine the correct answer and " +
      "verify each candidate's claims before grading — do not rely on prior knowledge when a tool can confirm the facts. " +
      "Follow the output format exactly."

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

  // Typed wire model for structured judge output. The opaque verdict keeps the
  // wire representation a plain "PASS" / "FAIL" JSON string while rejecting
  // other values during schema-derived decoding.
  opaque type JudgeVerdict = String
  object JudgeVerdict:
    val Pass: JudgeVerdict = "PASS"
    val Fail: JudgeVerdict = "FAIL"

    def parse(raw: String): Either[String, JudgeVerdict] =
      raw.toUpperCase match
        case "PASS" => Right(Pass)
        case "FAIL" => Right(Fail)
        case other  => Left(s"invalid judge verdict: $other")

    def render(v: JudgeVerdict): String = v

    given CanEqual[JudgeVerdict, JudgeVerdict] = CanEqual.derived
    given Schema[JudgeVerdict] =
      Schema[String].transformOrFail(parse, v => Right(render(v)))

  final case class JudgeGrade(arm: Int, verdict: JudgeVerdict, rationale: String) derives Schema
  final case class JudgeGrades(grades: List[JudgeGrade]) derives Schema

  // Derive the provider JSON Schema document from the SAME Schema used to decode
  // the response. Anthropic structured outputs require every object (including
  // nested grade items) to reject additional properties, so transform the typed
  // zio-http JsonSchema ADT before rendering it to JSON.
  val judgeSchema: Json =
    val rendered = strictObjects(
      JS.fromZSchema(
        summon[Schema[JudgeGrades]],
        JS.SchemaRef(JS.SchemaSpec.JsonSchema, JS.SchemaStyle.Inline),
      )
    ).toJson
    Json.decoder.decodeJson(rendered)
      .fold(error => throw IllegalStateException(s"could not render judge schema: $error"), identity)

  private def strictObjects(schema: JS): JS =
    schema match
      case o: JS.Object =>
        o.copy(
          properties           = o.properties.view.mapValues(strictObjects).toMap,
          additionalProperties = Left(false),
        )
      case a: JS.AnnotatedSchema => a.copy(schema = strictObjects(a.schema))
      case a: JS.AllOfSchema     => a.copy(allOf = a.allOf.map(strictObjects))
      case a: JS.AnyOfSchema     => a.copy(anyOf = a.anyOf.map(strictObjects))
      case a: JS.OneOfSchema     => a.copy(oneOf = a.oneOf.map(strictObjects))
      case a: JS.ArrayType       => a.copy(items = a.items.map(strictObjects))
      case other                 => other

  // Lenient extraction for CLIs that wrap otherwise valid structured JSON in prose.
  def sliceJson(s: String): String =
    val i = s.indexOf('{')
    val j = s.lastIndexOf('}')
    if i >= 0 && j > i then s.substring(i, j + 1) else s

  def parseJudge(text: String, n: Int): List[(EvalVerdict, String)] =
    val byArm = EvalCodecs.decode[JudgeGrades](sliceJson(text)).toOption
      .map(_.grades.map(g => g.arm -> g).toMap)
      .getOrElse(Map.empty)
    (1 to n).toList.map { i =>
      byArm.get(i) match
        case Some(g) =>
          val verdict = if g.verdict == JudgeVerdict.Pass then EvalVerdict.Pass else EvalVerdict.Fail
          (verdict, if g.rationale.nonEmpty then g.rationale else JudgeVerdict.render(g.verdict))
        case None => (EvalVerdict.Error, "judge returned no grade for this arm")
    }

  def incomplete(gs: List[(EvalVerdict, String)]): Boolean = gs.exists(_._1 == EvalVerdict.Error)

  def merge(first: List[(EvalVerdict, String)], retry: List[(EvalVerdict, String)]): List[(EvalVerdict, String)] =
    first.zip(retry).map { case (a, b) => if a._1 != EvalVerdict.Error then a else b }
