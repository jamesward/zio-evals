package com.jamesward.zio_evals

import zio.*

// The judge's per-arm verdicts (in input order) PLUS the judge agent's own
// transcript events, so a judge cell can show what the judge did.
final case class JudgeOutcome(verdicts: List[(EvalVerdict, String)], events: List[TranscriptEvent])

// Grades all the given (arm, answer) pairs for one eval against the rubric in a
// single call. A host can plug in its own judge (e.g. a hosted agent); the
// bundled default is `AgentLoopJudge`.
trait Judge:
  def judge(spec: EvalSpec, answers: List[(EvalArm, String)]): Task[JudgeOutcome]

// Default judge: drives an `AgentLoop` with `EvalJudging`'s prompt + schema,
// exposing `judgeServers` (the tools the judge uses to verify facts) with web
// off. Retries once if the model left any arm ungraded, then merges. Never
// fails the effect — a hard failure comes back as all-Error verdicts (surfaced,
// logged) so one bad judge call can't sink the whole run.
final class AgentLoopJudge(
    agentLoop:    AgentLoop,
    judgeModelId: String,
    judgeServers: List[McpServerConfig] = Nil,
) extends Judge:

  private val policy = AgentPolicy(web = false, toolSearch = false)

  def judge(spec: EvalSpec, answers: List[(EvalArm, String)]): Task[JudgeOutcome] =
    if answers.isEmpty then ZIO.succeed(JudgeOutcome(Nil, Nil))
    else
      val n      = answers.size
      val labels = answers.map((arm, ans) => arm.label -> ans)
      val prompt = s"${EvalJudging.judgeSystem}\n\n${EvalJudging.judgePrompt(spec, labels)}"
      val once: Task[(List[(EvalVerdict, String)], String)] =
        agentLoop.runStructured(prompt, judgeModelId, judgeServers, policy, EvalJudging.judgeSchema)
          .map(text => (EvalJudging.parseJudge(text, n), text))
      once.flatMap { (first, text) =>
        ZIO
          .when(EvalJudging.incomplete(first)):
            ZIO.logWarning(s"AgentLoopJudge: incomplete grades ($n arms), retrying once")
              *> once.map((retry, _) => EvalJudging.merge(first, retry))
          .map(_.getOrElse(first))
          .map(verdicts => JudgeOutcome(verdicts, List(TranscriptEvent.Note(text.take(4000)))))
      }.catchAllCause: c =>
        ZIO.logErrorCause("AgentLoopJudge: judge call failed", c)
          .as(JudgeOutcome(answers.map(_ => (EvalVerdict.Error, "judge failed")), List(TranscriptEvent.Note("judge failed"))))
