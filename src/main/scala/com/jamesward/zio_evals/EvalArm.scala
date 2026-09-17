package com.jamesward.zio_evals

import zio.schema.*

// One configuration under test in an eval run. Arms normally run the same task
// with different tools/skills, but may also override the task or system prompt
// when prompt/content variation is the independent variable.
//
//   * `name`         — stable identifier used as the transcript sample's arm key.
//   * `label`        — human-readable, shown in judge prompts and read surfaces.
//   * `systemPrompt` — optional per-run system prompt for supporting backends.
//   * `taskOverride` — optional replacement for the EvalSpec task in this arm.
final case class EvalArm(
    name:         String,
    label:        String,
    mcpServers:   List[McpServerConfig] = Nil,
    policy:       AgentPolicy = AgentPolicy.default,
    skills:       AgentSkills = AgentSkills.none,
    systemPrompt: Option[String] = None,
    taskOverride: Option[String] = None,
) derives Schema:
  def effectiveTask(defaultTask: String): String = taskOverride.getOrElse(defaultTask)

object EvalArm:
  given CanEqual[EvalArm, EvalArm] = CanEqual.derived

  // A bare "agent with nothing" control: no MCP servers, no web. The baseline
  // most eval suites compare their tool-enabled arms against.
  def modelOnly(
      name: String = "model",
      label: String = "Agent alone",
      systemPrompt: Option[String] = None,
      taskOverride: Option[String] = None,
  ): EvalArm =
    EvalArm(name, label, systemPrompt = systemPrompt, taskOverride = taskOverride)

  // An "agent with web tools" arm: no MCP servers, web enabled.
  def web(
      name: String = "web",
      label: String = "Agent with web",
      systemPrompt: Option[String] = None,
      taskOverride: Option[String] = None,
  ): EvalArm =
    EvalArm(name, label, policy = AgentPolicy(web = true), systemPrompt = systemPrompt, taskOverride = taskOverride)

  // An "agent with these MCP servers" arm (no web), the common "does my tool
  // help?" arm. `toolSearch` opts into MCP tool-search / deferred loading.
  def mcp(
      name: String,
      label: String,
      servers: List[McpServerConfig],
      toolSearch: Boolean = false,
      systemPrompt: Option[String] = None,
      taskOverride: Option[String] = None,
  ): EvalArm =
    EvalArm(
      name,
      label,
      servers,
      AgentPolicy(web = false, toolSearch = toolSearch),
      systemPrompt = systemPrompt,
      taskOverride = taskOverride,
    )

  // An agent augmented only with the supplied skills (no web or MCP tools).
  def withSkills(
      name: String,
      label: String,
      skills: AgentSkills,
      systemPrompt: Option[String] = None,
      taskOverride: Option[String] = None,
  ): EvalArm =
    EvalArm(name, label, skills = skills, systemPrompt = systemPrompt, taskOverride = taskOverride)
