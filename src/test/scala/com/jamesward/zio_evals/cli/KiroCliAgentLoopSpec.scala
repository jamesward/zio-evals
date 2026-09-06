package com.jamesward.zio_evals
package cli

import zio.test.*

object KiroCliAgentLoopSpec extends ZIOSpecDefault:

  private val loop = KiroCliAgentLoop()

  def spec = suite("KiroCliAgentLoop")(
    test("typed agentConfig for mcp arm wires the server and grants it, no web tool") {
      val cfg = loop.agentConfig("model-x", List(McpServerConfig("toolbook", "http://h/x", Map("X-Nonce" -> "n"))), AgentPolicy.default)
      assertTrue(
        cfg.tools.contains("@toolbook"),
        cfg.allowedTools.contains("@toolbook"),
        !cfg.includeMcpJson,
        cfg.mcpServers("toolbook").url == "http://h/x",
        cfg.mcpServers("toolbook").headers.get("X-Nonce").contains("n"),
        cfg.model.contains("model-x"),
        cfg.resources.isEmpty,
      )
    },
    test("typed agentConfig for web arm grants web_fetch and has no mcp servers") {
      val cfg = loop.agentConfig("m", Nil, AgentPolicy(web = true))
      assertTrue(cfg.tools.contains("web_fetch"), cfg.allowedTools.contains("web_fetch"), cfg.mcpServers.isEmpty)
    },
    test("agent config round-trips through its derived Schema codec") {
      val cfg     = loop.agentConfig("m", List(McpServerConfig("s", "http://h")), AgentPolicy.default)
      val decoded = EvalCodecs.decode[KiroAgentConfig](EvalCodecs.encode(cfg))
      assertTrue(decoded.isRight, decoded.toOption.get.name == "eval", decoded.toOption.get.mcpServers.contains("s"))
    },
    test("cliArgs uses v2 stream-json + agent + model, and require-mcp-startup when asked") {
      val args = loop.cliArgs("do it", "model-x", requireMcpStartup = true)
      assertTrue(
        args.head == "chat",
        args.contains("--agent-engine") && args(args.indexOf("--agent-engine") + 1) == "v2",
        args.contains("--output-format") && args(args.indexOf("--output-format") + 1) == "stream-json",
        !args.exists(_.startsWith("--trust")),
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
      val cfg = KiroCliAgentLoop(modelOverride = Some("pinned")).agentConfig("ignored", Nil, AgentPolicy.default)
      assertTrue(cfg.model.contains("pinned"))
    },
    test("agent config contains only explicitly supplied skill resources") {
      val uri = "skill:///tmp/eval/.kiro/skills/zen-of-james/SKILL.md"
      val cfg = loop.agentConfig("m", Nil, AgentPolicy.default, List(uri))
      assertTrue(cfg.resources == List(uri), cfg.tools.isEmpty, cfg.mcpServers.isEmpty)
    },
    test("explicit skills are file context; available skills remain progressive") {
      val skill = MaterializedSkill("zen-of-james", java.nio.file.Paths.get("/tmp/zen-of-james"))
      assertTrue(
        loop.skillResourceUris(List(skill), SkillActivation.Explicit) == List("file:///tmp/zen-of-james/SKILL.md"),
        loop.skillResourceUris(List(skill), SkillActivation.Available) == List("skill:///tmp/zen-of-james/SKILL.md"),
      )
    },
  )
