package com.jamesward.zio_evals
package cli

import zio.*
import zio.test.*
import zio.test.TestAspect.*

// Real, paid CLI smoke tests. They live in the normal `src/test` suite, so a
// regular test run executes them whenever the corresponding CLI is installed,
// authenticated, and working; `RealCliTestGates` ignores them otherwise.
//
// Keep the prompts tiny: these tests prove the bundled `AgentLoop` backends can
// complete a real inference and return the final answer, not model quality.
object RealCliSpec extends ZIOSpecDefault:

  private val claudeMarker = "ZIO-EVALS-CLAUDE-OK-7K3"
  private val kiroMarker   = "ZIO-EVALS-KIRO-OK-9P2"

  private def containsMarker(result: AgentRunResult, marker: String): Boolean =
    result.answer.toUpperCase.contains(marker)

  def spec = suite("real CLI backends")(
    test("ClaudeCliAgentLoop completes a real claude -p inference") {
      val backend = ClaudeCliAgentLoop(
        modelOverride = Some("claude-sonnet-4-6"),
        maxBudgetUsd  = "0.10",
        runTimeout    = 90.seconds,
      )
      backend
        .run(
          s"Reply with exactly this text and nothing else: $claudeMarker",
          "claude-sonnet-4-6",
          Nil,
          AgentPolicy.default,
        )
        .map(result => assertTrue(
          containsMarker(result, claudeMarker),
          result.iterations > 0,
          result.outputTokens > 0,
          result.latencyMs > 0,
        ))
    } @@ RealCliTestGates.ifClaudeAvailable,

    test("KiroCliAgentLoop completes a real kiro-cli chat inference") {
      val backend = KiroCliAgentLoop(runTimeout = 120.seconds)
      backend
        .run(
          s"Reply with exactly this text and nothing else: $kiroMarker",
          "", // use the operator's working/default kiro-cli model
          Nil,
          AgentPolicy.default,
        )
        .map(result => assertTrue(
          containsMarker(result, kiroMarker),
          result.latencyMs > 0,
          result.events.exists {
            case TranscriptEvent.AgentMessage(text) => text.toUpperCase.contains(kiroMarker)
            case _                                  => false
          },
        ))
    } @@ RealCliTestGates.ifKiroAvailable,
  ) @@ withLiveClock @@ timeout(300.seconds) @@ sequential
