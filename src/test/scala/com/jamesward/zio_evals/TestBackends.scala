package com.jamesward.zio_evals

import zio.*
import zio.json.ast.Json

// Deterministic stand-ins so the runner/judge pipeline can be exercised without
// shelling out to a real CLI or making a paid model call.
//
// `run`: when the arm exposes MCP servers, the "agent" pretends to call the
// first server's tool (emitting a ToolCall event + an answer that mentions the
// tool) so transcript capture and `ToolCalled`/`AnswerContains` checks have
// something real to assert on; with no servers it answers plainly.
//
// `runStructured` (the judge path): emit a canned `{grades:[...]}` with one
// PASS per "--- Arm n ---" marker in the prompt, so `AgentLoopJudge` parses
// valid verdicts.
final class FakeAgentLoop extends AgentLoop:

  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    mcpServers.headOption match
      case Some(server) =>
        val tool = s"mcp__${server.name}__search"
        val out  = s"Used tool $tool to find the answer"
        val events = List(
          TranscriptEvent.Thinking("I should consult the tool."),
          TranscriptEvent.ToolCall(tool, "{}"),
          TranscriptEvent.ToolResult(tool, "found it", isError = false),
          TranscriptEvent.AgentMessage(out),
        )
        ZIO.succeed(AgentRunResult(out, iterations = 1, toolCalls = 1, inputTokens = 10, outputTokens = 10, latencyMs = 5, events))
      case None =>
        ZIO.succeed(AgentRunResult("no tools available", 1, 0, 10, 10, 5, List(TranscriptEvent.AgentMessage("no tools available"))))

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    val n      = math.max(1, "--- Arm ".r.findAllIn(prompt).size)
    val grades = (1 to n).map(i => s"""{"arm":$i,"verdict":"PASS","rationale":"matches the rubric"}""").mkString(",")
    ZIO.succeed(s"""{"grades":[$grades]}""")

// A backend that fails every `run` (to exercise the runner's failure path).
final class FailingAgentLoop extends AgentLoop:
  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    ZIO.fail(RuntimeException("boom"))
  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    ZIO.succeed("""{"grades":[]}""")
