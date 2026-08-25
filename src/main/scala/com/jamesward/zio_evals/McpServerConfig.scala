package com.jamesward.zio_evals

import zio.json.ast.Json
import zio.json.EncoderOps
import zio.schema.*

// A single MCP server an eval arm exposes to the agent, reached over HTTP. This
// is the generic replacement for a host-specific "expose exactly this one
// server" flag: an arm carries a list of these, and the CLI backends render
// them into whatever config shape their CLI expects.
//
// `name` is the server label the agent sees (tool names are namespaced under
// it, e.g. `mcp__<name>__<tool>` for claude, `@<name>/<tool>` for kiro-cli).
// `headers` carries auth (a bearer token, a nonce header, …); empty by default.
final case class McpServerConfig(
    name:    String,
    url:     String,
    headers: Map[String, String] = Map.empty,
) derives Schema

object McpServerConfig:
  given CanEqual[McpServerConfig, McpServerConfig] = CanEqual.derived

  // The `--mcp-config` JSON the `claude` CLI expects:
  // {"mcpServers":{"<name>":{"type":"http","url":"...","headers":{...}}}}.
  // Headers are omitted when empty so the common no-auth case stays minimal.
  def claudeMcpConfigJson(servers: List[McpServerConfig]): String =
    val entries = servers.map { s =>
      val base: List[(String, Json)] =
        List("type" -> Json.Str("http"), "url" -> Json.Str(s.url))
      val withHeaders =
        if s.headers.isEmpty then base
        else base :+ ("headers" -> Json.Obj(s.headers.toList.map((k, v) => k -> Json.Str(v))*))
      s.name -> Json.Obj(withHeaders*)
    }
    Json.Obj("mcpServers" -> Json.Obj(entries*)).toJson

  // The `mcpServers` object embedded in a kiro-cli agent config
  // (`.kiro/agents/<name>.json`): remote HTTP servers are `{url, headers?}`.
  def kiroAgentMcpJson(servers: List[McpServerConfig]): Json =
    val entries = servers.map { s =>
      val base: List[(String, Json)] = List("url" -> Json.Str(s.url))
      val withHeaders =
        if s.headers.isEmpty then base
        else base :+ ("headers" -> Json.Obj(s.headers.toList.map((k, v) => k -> Json.Str(v))*))
      s.name -> Json.Obj(withHeaders*)
    }
    Json.Obj(entries*)
