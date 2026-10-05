package com.jamesward.zio_evals

import zio.*
import zio.json.ast.Json

// One normalized tool invocation captured from a backend transcript. `input` is
// the raw JSON arguments text supplied by that backend.
final case class CapturedToolCall(name: String, input: String)

object CapturedToolCall:
  given CanEqual[CapturedToolCall, CapturedToolCall] = CanEqual.derived

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
    // Breakdown of `inputTokens` (which already includes both) for backends
    // that report prompt caching, the backend-reported cost, and the session id
    // a later `AgentRunRequest.resumeSessionId` can continue. 0 / None when the
    // backend does not report them.
    cacheReadTokens:     Long = 0,
    cacheCreationTokens: Long = 0,
    costUsd:             Double = 0.0,
    sessionId:           Option[String] = None,
):
  // The normalized calls captured by any backend that emits ToolCall events.
  // Keeping this derived from `events` makes the transcript the single source
  // of truth and gives custom backends the same evaluation behavior.
  def capturedToolCalls: List[CapturedToolCall] =
    events.collect { case TranscriptEvent.ToolCall(name, input) => CapturedToolCall(name, input) }

// Per-arm controls the runner sets when driving an arm/judge through the seam.
// `web` gates the agent's built-in web search/fetch tools; `toolSearch` enables
// MCP tool search / deferred loading (the model discovers MCP tools on demand
// instead of all schemas loading up front); `coding` grants the backend's
// built-in coding tools (shell, file read/write/edit, search). Grant `coding`
// only when the agent runs inside a sandboxed workspace. Model selection is the
// runner's concern (passed to `run` per model id), so it is NOT in the policy.
final case class AgentPolicy(web: Boolean = false, toolSearch: Boolean = false, coding: Boolean = false)

object AgentPolicy:
  // No web tools, no tool search — the safe baseline. Callers opt in explicitly.
  val default: AgentPolicy = AgentPolicy()

// One agent invocation. `workspace` runs the agent inside a sandbox (see
// `Sandbox`); `resumeSessionId` continues a previous run's conversation (from
// `AgentRunResult.sessionId`) so follow-up feedback keeps the agent's context;
// `timeout` overrides the backend's default run timeout.
final case class AgentRunRequest(
    prompt:          String,
    modelId:         String,
    mcpServers:      List[McpServerConfig] = Nil,
    policy:          AgentPolicy = AgentPolicy.default,
    skills:          AgentSkills = AgentSkills.none,
    systemPrompt:    Option[String] = None,
    workspace:       Option[Workspace] = None,
    resumeSessionId: Option[String] = None,
    timeout:         Option[Duration] = None,
)

// The provider-agnostic seam an eval arm runs against: a prompt plus the MCP
// servers to expose + an `AgentPolicy` and a model id, in; an answer plus
// efficiency metrics out. The MCP servers are reached over HTTP, so there is no
// in-process tool bridge — callers depend only on this trait. Implemented by
// the bundled CLI backends (`ClaudeCliAgentLoop`, `KiroCliAgentLoop`) and by
// hosts that plug in their own agent (e.g. a hosted-agent backend).
trait AgentLoop:
  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult]

  // Skills-aware overload. Existing third-party AgentLoop implementations stay
  // source compatible and continue to support skill-free arms; backends that
  // can isolate and load skills override this method.
  def run(
      prompt: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
  ): Task[AgentRunResult] =
    if skills.isEmpty then run(prompt, modelId, mcpServers, policy)
    else ZIO.fail(UnsupportedOperationException("this AgentLoop backend does not support agent skills"))

  // Per-run system-prompt overload used by EvalArm. It defaults through the
  // existing skills-aware seam, preserving source compatibility for custom
  // backends. A backend must opt in before accepting a non-empty system prompt.
  def run(
      prompt: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
      systemPrompt: Option[String],
  ): Task[AgentRunResult] =
    systemPrompt.filter(_.nonEmpty) match
      case None    => run(prompt, modelId, mcpServers, policy, skills)
      case Some(_) => ZIO.fail(UnsupportedOperationException("this AgentLoop backend does not support per-run system prompts"))

  // The general entry point. A request without a workspace or resume session
  // delegates to the six-argument `run`, so every existing backend supports
  // it. Running inside a sandboxed `Workspace` (the agent process itself
  // executes there, so its tools see only the workspace) or resuming a prior
  // session requires a backend that overrides this method.
  def run(request: AgentRunRequest): Task[AgentRunResult] =
    if request.workspace.nonEmpty then
      ZIO.fail(UnsupportedOperationException("this AgentLoop backend cannot run inside a sandbox workspace"))
    else if request.resumeSessionId.nonEmpty then
      ZIO.fail(UnsupportedOperationException("this AgentLoop backend cannot resume sessions"))
    else run(request.prompt, request.modelId, request.mcpServers, request.policy, request.skills, request.systemPrompt)

  // Like `run`, but constrains the FINAL output to `schema` and returns ONLY
  // that structured JSON text (the judge's use — one structured verdict set
  // over all arms). A backend that can't constrain output should ask for the
  // shape in the prompt and return its best-effort text (the judge parser is
  // lenient). The agent may still use the MCP tools to reach the answer.
  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String]

object AgentLoop:
  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): RIO[AgentLoop, AgentRunResult] =
    ZIO.serviceWithZIO[AgentLoop](_.run(prompt, modelId, mcpServers, policy))

  def run(
      prompt: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
  ): RIO[AgentLoop, AgentRunResult] =
    ZIO.serviceWithZIO[AgentLoop](_.run(prompt, modelId, mcpServers, policy, skills))

  def run(
      prompt: String,
      modelId: String,
      mcpServers: List[McpServerConfig],
      policy: AgentPolicy,
      skills: AgentSkills,
      systemPrompt: Option[String],
  ): RIO[AgentLoop, AgentRunResult] =
    ZIO.serviceWithZIO[AgentLoop](_.run(prompt, modelId, mcpServers, policy, skills, systemPrompt))

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): RIO[AgentLoop, String] =
    ZIO.serviceWithZIO[AgentLoop](_.runStructured(prompt, modelId, mcpServers, policy, schema))
