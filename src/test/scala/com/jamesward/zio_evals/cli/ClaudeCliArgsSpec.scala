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
  )
