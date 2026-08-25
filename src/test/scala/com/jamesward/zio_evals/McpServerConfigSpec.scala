package com.jamesward.zio_evals

import zio.json.ast.Json
import zio.test.*

object McpServerConfigSpec extends ZIOSpecDefault:

  def spec = suite("McpServerConfig")(
    test("claudeMcpConfigJson renders http servers, omitting empty headers") {
      val json = McpServerConfig.claudeMcpConfigJson(List(McpServerConfig("toolbook", "http://localhost:8080/x")))
      val parsed = Json.decoder.decodeJson(json).toOption.get
      val server = parsed.asObject.get.get("mcpServers").get.asObject.get.get("toolbook").get.asObject.get
      assertTrue(
        server.get("type").get.asString.contains("http"),
        server.get("url").get.asString.contains("http://localhost:8080/x"),
        server.get("headers").isEmpty,
      )
    },
    test("claudeMcpConfigJson includes headers when present") {
      val json = McpServerConfig.claudeMcpConfigJson(List(McpServerConfig("tb", "http://h/x", Map("Authorization" -> "Bearer t"))))
      val server = Json.decoder.decodeJson(json).toOption.get.asObject.get
        .get("mcpServers").get.asObject.get.get("tb").get.asObject.get
      assertTrue(server.get("headers").get.asObject.get.get("Authorization").get.asString.contains("Bearer t"))
    },
    test("kiroAgentMcpJson renders url + headers per server") {
      val json = McpServerConfig.kiroAgentMcpJson(List(McpServerConfig("tb", "http://h/x", Map("X-Nonce" -> "n"))))
      val tb = json.asObject.get.get("tb").get.asObject.get
      assertTrue(
        tb.get("url").get.asString.contains("http://h/x"),
        tb.get("headers").get.asObject.get.get("X-Nonce").get.asString.contains("n"),
      )
    },
  )
