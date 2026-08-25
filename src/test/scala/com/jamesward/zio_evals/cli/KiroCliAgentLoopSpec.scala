package com.jamesward.zio_evals
package cli

import zio.test.*

object KiroCliAgentLoopSpec extends ZIOSpecDefault:

  private val loop = KiroCliAgentLoop()

  def spec = suite("KiroCliAgentLoop")(
    test("agentConfig for mcp arm wires the server and grants it, no web tool") {
      val cfg = loop.agentConfig("model-x", List(McpServerConfig("toolbook", "http://h/x", Map("X-Nonce" -> "n"))), AgentPolicy.default).asObject.get
      val tools   = cfg.get("tools").get.asArray.get.flatMap(_.asString)
      val allowed = cfg.get("allowedTools").get.asArray.get.flatMap(_.asString)
      val servers = cfg.get("mcpServers").get.asObject.get
      assertTrue(
        tools.isEmpty,                                    // no web => no builtin tools
        allowed.contains("@toolbook"),
        cfg.get("includeMcpJson").get.asBoolean.contains(false),
        servers.get("toolbook").get.asObject.get.get("url").get.asString.contains("http://h/x"),
        cfg.get("model").get.asString.contains("model-x"),
      )
    },
    test("agentConfig for web arm grants web_fetch and configures no mcp servers") {
      val cfg = loop.agentConfig("m", Nil, AgentPolicy(web = true)).asObject.get
      val tools   = cfg.get("tools").get.asArray.get.flatMap(_.asString)
      val allowed = cfg.get("allowedTools").get.asArray.get.flatMap(_.asString)
      assertTrue(tools.contains("web_fetch"), allowed.contains("web_fetch"), cfg.get("mcpServers").isEmpty)
    },
    test("cliArgs uses headless + trust-all + agent + model, and require-mcp-startup when asked") {
      val args = loop.cliArgs("do it", "model-x", requireMcpStartup = true)
      assertTrue(
        args.head == "chat",
        args.contains("--no-interactive"),
        args.contains("--trust-all-tools"),
        args.contains("--require-mcp-startup"),
        args.contains("--agent") && args(args.indexOf("--agent") + 1) == "eval",
        args.contains("--model") && args(args.indexOf("--model") + 1) == "model-x",
        args.last == "do it",
      )
    },
    test("cliArgs omits --require-mcp-startup and --model when not needed") {
      val args = KiroCliAgentLoop().cliArgs("q", "", requireMcpStartup = false)
      assertTrue(!args.contains("--require-mcp-startup"), !args.contains("--model"))
    },
    test("modelOverride wins over run model id") {
      val cfg = KiroCliAgentLoop(modelOverride = Some("pinned")).agentConfig("ignored", Nil, AgentPolicy.default).asObject.get
      assertTrue(cfg.get("model").get.asString.contains("pinned"))
    },
  )
