package com.jamesward.zio_evals

import zio.*
import zio.json.ast.Json

// The answer plus the efficiency metrics an eval records per (arm x model):
// iterations, tool calls, tokens, wall-time. `events` is the full turn-by-turn
// conversation (agent messages, thinking, tool calls + their results) captured
// for the transcript — empty only if the backend exposed no per-turn detail
// (e.g. a plain-text CLI). A backend that has no notion of a metric reports 0.
final case class AgentRunResult(
    answer:       String,
    iterations:   Int,
    toolCalls:    Int,
    inputTokens:  Long,
    outputTokens: Long,
    latencyMs:    Long,
    events:       List[TranscriptEvent] = Nil,
)

// Per-arm controls the runner sets when driving an arm/judge through the seam.
// `web` gates the agent's built-in web search/fetch tools; `toolSearch` enables
// MCP tool search / deferred loading (the model discovers MCP tools on demand
// instead of all schemas loading up front). Model selection is the runner's
// concern (passed to `run` per model id), so it is NOT part of the policy.
final case class AgentPolicy(web: Boolean = false, toolSearch: Boolean = false)

object AgentPolicy:
  // No web tools, no tool search — the safe baseline. Callers opt in explicitly.
  val default: AgentPolicy = AgentPolicy()

// The provider-agnostic seam an eval arm runs against: a prompt plus the MCP
// servers to expose + an `AgentPolicy` and a model id, in; an answer plus
// efficiency metrics out. The MCP servers are reached over HTTP, so there is no
// in-process tool bridge — callers depend only on this trait. Implemented by
// the bundled CLI backends (`ClaudeCliAgentLoop`, `KiroCliAgentLoop`) and by
// hosts that plug in their own agent (e.g. a hosted-agent backend).
trait AgentLoop:
  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult]

  // Like `run`, but constrains the FINAL output to `schema` and returns ONLY
  // that structured JSON text (the judge's use — one structured verdict set
  // over all arms). A backend that can't constrain output should ask for the
  // shape in the prompt and return its best-effort text (the judge parser is
  // lenient). The agent may still use the MCP tools to reach the answer.
  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String]

object AgentLoop:
  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): RIO[AgentLoop, AgentRunResult] =
    ZIO.serviceWithZIO[AgentLoop](_.run(prompt, modelId, mcpServers, policy))

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): RIO[AgentLoop, String] =
    ZIO.serviceWithZIO[AgentLoop](_.runStructured(prompt, modelId, mcpServers, policy, schema))
