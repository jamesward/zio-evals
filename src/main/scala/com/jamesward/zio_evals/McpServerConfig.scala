package com.jamesward.zio_evals

import zio.schema.*
import zio.schema.annotation.fieldName

// A single MCP server an eval arm exposes to the agent, reached over HTTP.
// `name` is the server label the agent sees; `headers` carries optional auth.
final case class McpServerConfig(
    name:    String,
    url:     String,
    headers: Map[String, String] = Map.empty,
) derives Schema

object McpServerConfig:
  given CanEqual[McpServerConfig, McpServerConfig] = CanEqual.derived

  // Typed wire records for the two CLI config formats. Encoding is entirely
  // schema-derived via `EvalCodecs`; there is no hand-built JSON object.
  final case class ClaudeMcpServer(
      @fieldName("type") transport: String,
      url:                           String,
      headers:                       Map[String, String],
  ) derives Schema

  final case class ClaudeMcpConfig(
      mcpServers: Map[String, ClaudeMcpServer]
  ) derives Schema

  final case class KiroMcpServer(
      url:     String,
      headers: Map[String, String],
  ) derives Schema

  def claudeMcpConfig(servers: List[McpServerConfig]): ClaudeMcpConfig =
    ClaudeMcpConfig(
      servers.map(s => s.name -> ClaudeMcpServer("http", s.url, s.headers)).toMap
    )

  def claudeMcpConfigJson(servers: List[McpServerConfig]): String =
    EvalCodecs.encode(claudeMcpConfig(servers))

  def kiroMcpServers(servers: List[McpServerConfig]): Map[String, KiroMcpServer] =
    servers.map(s => s.name -> KiroMcpServer(s.url, s.headers)).toMap
