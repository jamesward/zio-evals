package com.jamesward.zio_evals

import zio.schema.*

// One configuration under test in an eval run. An eval is scored by running the
// SAME task through several arms and comparing — e.g. "agent + my MCP server"
// vs "agent + web" vs "agent alone" — so results are directly comparable and
// isolate the marginal value of the tools an arm is given.
//
// An arm is purely a bundle of what the agent gets: which MCP servers to expose
// and the `AgentPolicy` (web / tool-search). The runner drives every arm
// through the same `AgentLoop`; the only difference between arms is this bundle.
//
//   * `name`  — stable identifier used as the transcript sample's `arm` key.
//   * `label` — human-readable, shown in the judge prompt and read surfaces.
final case class EvalArm(
    name:       String,
    label:      String,
    mcpServers: List[McpServerConfig] = Nil,
    policy:     AgentPolicy = AgentPolicy.default,
) derives Schema

object EvalArm:
  given CanEqual[EvalArm, EvalArm] = CanEqual.derived

  // A bare "agent with nothing" control: no MCP servers, no web. The baseline
  // most eval suites compare their tool-enabled arms against.
  def modelOnly(name: String = "model", label: String = "Agent alone"): EvalArm =
    EvalArm(name, label)

  // An "agent with web tools" arm: no MCP servers, web enabled.
  def web(name: String = "web", label: String = "Agent with web"): EvalArm =
    EvalArm(name, label, policy = AgentPolicy(web = true))

  // An "agent with these MCP servers" arm (no web), the common "does my tool
  // help?" arm. `toolSearch` opts into MCP tool-search / deferred loading.
  def mcp(name: String, label: String, servers: List[McpServerConfig], toolSearch: Boolean = false): EvalArm =
    EvalArm(name, label, servers, AgentPolicy(web = false, toolSearch = toolSearch))
