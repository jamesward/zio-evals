package com.jamesward.zio_evals
package acp

import com.jamesward.zio_evals.cli.RealCliTestGates
import com.jamesward.zioacp.AgentCommand
import zio.*
import zio.test.*

import java.nio.file.Files

// Paid, real ACP agents through AcpAgentLoop: Kiro (`kiro-cli acp`) and Claude Code through its ACP adapter
// (`npx @agentclientprotocol/claude-agent-acp`, which needs `ZIO_EVALS_CLAUDE_ACP=true` since it downloads the
// adapter). Each reads a file in a temporary project with its own tools and answers from it. Ignored when the agent
// isn't available.
object RealAcpSpec extends ZIOSpecDefault:

  private def readsAFile(loop: AcpAgentLoop, model: String) =
    for
      dir    <- ZIO.attemptBlocking(Files.createTempDirectory("acp-real"))
      _      <- ZIO.attemptBlocking(Files.writeString(dir.resolve("build.sbt"), "scalaVersion := \"3.7.2\"\n"))
      result <- loop.runIn("What Scala version does build.sbt set? Answer with just the version.", model, AcpAgentLoop.Session(dir))
      _      <- Console.printLine(s"${result.events.headOption}; ${result.toolCalls} tool calls, ${result.latencyMs} ms, " +
                  s"${result.inputTokens} in / ${result.outputTokens} out tokens, $$${result.costUsd}")
    yield assertTrue(result.answer.contains("3.7.2"), result.toolCalls >= 1, result.capturedToolCalls.nonEmpty)

  def spec = suite("real ACP agents")(
    test("Kiro reads a file and answers") {
      readsAFile(AcpAgentLoop(AgentCommand("kiro-cli", List("acp"))), "")
    } @@ RealCliTestGates.ifKiroAvailable,
    test("Claude Code (ACP adapter) reads a file and answers, reporting usage") {
      readsAFile(AcpAgentLoop(AgentCommand("npx", List("-y", "@agentclientprotocol/claude-agent-acp@0.89.1"))), "")
        .map(_ && assertCompletes)
    } @@ TestAspect.ifEnv("ZIO_EVALS_CLAUDE_ACP")(_ == "true"),
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(10.minutes) @@ TestAspect.sequential
