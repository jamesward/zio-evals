package com.jamesward.zio_evals
package cli

import zio.test.*

object ClaudeCliArgsSpec extends ZIOSpecDefault:

  private val loop = ClaudeCliAgentLoop()

  private def slice(args: List[String], flag: String): List[String] =
    args.dropWhile(_ != flag).drop(1).takeWhile(a => !a.startsWith("--"))

  def spec = suite("ClaudeCliAgentLoop args")(
    test("model-only arm: no mcp-config, web tools disallowed, no allowedTools") {
      val args = loop.cliArgsFor("q", "haiku", Nil, AgentPolicy.default)
      assertTrue(
        !args.contains("--mcp-config"),
        !args.contains("--allowedTools"),
        args.contains("--disallowedTools"),
        ClaudeCliAgentLoop.webTools.forall(args.contains),
        args.contains("--model") && args(args.indexOf("--model") + 1) == "haiku",
      )
    },
    test("web arm: web tools allowed, not disallowed") {
      val args = loop.cliArgsFor("q", "m", Nil, AgentPolicy(web = true))
      val allowed = slice(args, "--allowedTools")
      val disallowed = slice(args, "--disallowedTools")
      assertTrue(
        allowed.contains("WebFetch") && allowed.contains("WebSearch"),
        !disallowed.contains("WebFetch"),
      )
    },
    test("mcp arm: per-server grant + mcp-config present") {
      val args = loop.cliArgsFor("q", "m", List(McpServerConfig("toolbook", "http://h/x"), McpServerConfig("db", "http://h/d")), AgentPolicy.default)
      val allowed = slice(args, "--allowedTools")
      assertTrue(
        args.contains("--mcp-config"),
        allowed.contains("mcp__toolbook"),
        allowed.contains("mcp__db"),
      )
    },
    test("judge arm: --json-schema present") {
      val args = loop.cliArgsFor("q", "m", Nil, AgentPolicy.default, Some(EvalJudging.judgeSchema))
      assertTrue(args.contains("--json-schema"))
    },
    test("system prompt uses the supported append flag and per-run override wins") {
      val configured = ClaudeCliAgentLoop(systemPrompt = Some("base instructions"))
      val baseArgs = configured.cliArgsFor("q", "m", Nil, AgentPolicy.default)
      val overrideArgs = configured.cliArgsFor(
        "q",
        "m",
        Nil,
        AgentPolicy.default,
        systemPromptOverride = Some("arm instructions"),
      )
      assertTrue(
        baseArgs(baseArgs.indexOf("--append-system-prompt") + 1) == "base instructions",
        overrideArgs(overrideArgs.indexOf("--append-system-prompt") + 1) == "arm instructions",
      )
    },
    test("modelOverride pins the model regardless of run model id") {
      val args = ClaudeCliAgentLoop(modelOverride = Some("sonnet")).cliArgsFor("q", "haiku", Nil, AgentPolicy.default)
      assertTrue(args(args.indexOf("--model") + 1) == "sonnet")
    },
    test("allowShell grants Bash and removes it from disallowed (agent can choose shell vs MCP)") {
      val args = ClaudeCliAgentLoop(allowShell = true).cliArgsFor("q", "m", List(McpServerConfig("sbtmcp", "http://h/x")), AgentPolicy.default)
      val allowed    = slice(args, "--allowedTools")
      val disallowed = slice(args, "--disallowedTools")
      assertTrue(
        allowed.contains("Bash"),
        allowed.contains("mcp__sbtmcp"),
        !disallowed.contains("Bash"),
      )
    },
    test("skills arm loads project settings and grants only the named skill") {
      val skills = AgentSkills.explicit(AgentSkill("zen-of-james", SkillSource.Classpath("zen/SKILL.md")))
      val args = loop.cliArgsFor("q", "m", Nil, AgentPolicy.default, skills = skills)
      val allowed = slice(args, "--allowedTools")
      val disallowed = slice(args, "--disallowedTools")
      val settingSources = args(args.indexOf("--setting-sources") + 1)
      assertTrue(
        settingSources == "project",
        allowed.contains("Skill(zen-of-james)"),
        !disallowed.contains("Skill"),
        ClaudeCliAgentLoop.webTools.forall(disallowed.contains),
      )
    },
  )
