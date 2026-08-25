package com.jamesward.zio_evals

import zio.test.*

object McpServerConfigSpec extends ZIOSpecDefault:

  def spec = suite("McpServerConfig")(
    test("Claude config is a typed HTTP server config") {
      val cfg = McpServerConfig.claudeMcpConfig(List(McpServerConfig("toolbook", "http://localhost:8080/x")))
      val server = cfg.mcpServers("toolbook")
      assertTrue(
        server.transport == "http",
        server.url == "http://localhost:8080/x",
        server.headers.isEmpty,
      )
    },
    test("Claude config schema JSON round-trips headers") {
      val original = McpServerConfig.claudeMcpConfig(List(McpServerConfig("tb", "http://h/x", Map("Authorization" -> "Bearer t"))))
      val decoded  = EvalCodecs.decode[McpServerConfig.ClaudeMcpConfig](EvalCodecs.encode(original))
      assertTrue(
        decoded.isRight,
        decoded.toOption.get.mcpServers("tb").headers.get("Authorization").contains("Bearer t"),
      )
    },
    test("Kiro config is a typed url + headers map") {
      val servers = McpServerConfig.kiroMcpServers(List(McpServerConfig("tb", "http://h/x", Map("X-Nonce" -> "n"))))
      assertTrue(
        servers("tb").url == "http://h/x",
        servers("tb").headers.get("X-Nonce").contains("n"),
      )
    },
  )
