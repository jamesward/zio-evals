package com.jamesward.zio_evals
package acp

import com.jamesward.zioacp.{AcpAgent, AcpClient, Agent, ClientConnection, PromptTurn, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object AcpAgentLoopSpec extends ZIOSpecDefault:

  // What the scripted agent saw.
  final case class Seen(cwd: String, mcpServers: List[McpServer], meta: Option[Json.Obj], model: Option[String], prompt: String,
      permission: Option[RequestPermissionOutcome])

  private def modelOption(current: String) = SessionConfigOption.Select(
    SessionConfigId("model"), "Model", SessionConfigValueId(current),
    SessionConfigSelectOptions.Ungrouped(List("small", "large").map(m => SessionConfigSelectOption(SessionConfigValueId(m), m))),
    category = Some(SessionConfigOptionCategory.Model),
  )

  // An agent that thinks, reads a file (asking permission), fails a search, answers in two chunks and reports usage.
  private final class ScriptedAcpAgent(client: ClientConnection, seen: Ref[Seen], withModelOption: Boolean) extends Agent:
    def initialize(request: InitializeRequest): IO[RpcError, InitializeResponse] =
      ZIO.succeed(InitializeResponse(ProtocolVersion.V1, agentInfo = Some(Implementation("scripted", "1.0")),
        agentCapabilities = AgentCapabilities(sessionCapabilities = SessionCapabilities(close = Some(Marker())))))
    def newSession(request: NewSessionRequest): IO[RpcError, NewSessionResponse] =
      seen.update(_.copy(cwd = request.cwd, mcpServers = request.mcpServers, meta = request.meta)).as:
        NewSessionResponse(SessionId("s1"), configOptions = Option.when(withModelOption)(List(modelOption("small"))))
    override def setSessionConfigOption(request: SetSessionConfigOptionRequest): IO[RpcError, SetSessionConfigOptionResponse] =
      request.value match
        case SessionConfigValue.ValueId(v) => seen.update(_.copy(model = Some(v.value))).as(SetSessionConfigOptionResponse(List(modelOption(v.value))))
        case _                             => ZIO.fail(RpcError.invalidParams("not a value"))
    override def extMethod(method: String, params: Json): IO[RpcError, Json] =
      if method == "session/set_model" then
        seen.update(_.copy(model = params.asObject.flatMap(_.get("modelId")).flatMap(_.asString))).as(Json.Obj())
      else ZIO.fail(RpcError.methodNotFound(method))
    override def closeSession(request: CloseSessionRequest): IO[RpcError, CloseSessionResponse] = ZIO.succeed(CloseSessionResponse())
    def prompt(request: PromptRequest, turn: PromptTurn): IO[RpcError, PromptResponse] =
      val sid = request.sessionId
      def say(update: SessionUpdate) = client.sessionUpdate(sid, update)
      val read = ToolCallId("t1")
      for
        _ <- seen.update(_.copy(prompt = request.prompt.collect { case ContentBlock.Text(t, _, _) => t }.mkString))
        _ <- say(SessionUpdate.AgentThoughtChunk(ContentBlock.text("Let me ")))
        _ <- say(SessionUpdate.AgentThoughtChunk(ContentBlock.text("look.")))
        _ <- say(SessionUpdate.ToolCallStarted(ToolCall(read, "Read build.sbt", name = Some("Read"), kind = ToolKind.Read)))
        // the input arrives in an update, as Claude's adapter sends it
        _ <- say(SessionUpdate.ToolCallUpdated(ToolCallUpdate(read, rawInput = Some(Json.Obj("file_path" -> Json.Str("build.sbt"))))))
        answer <- client.requestPermission(RequestPermissionRequest(sid, ToolCallUpdate(read), List(
                    PermissionOption(PermissionOptionId("no"), "Reject", PermissionOptionKind.RejectOnce),
                    PermissionOption(PermissionOptionId("yes"), "Allow", PermissionOptionKind.AllowOnce),
                  )))
        _ <- seen.update(_.copy(permission = Some(answer.outcome)))
        _ <- say(SessionUpdate.ToolCallUpdated(ToolCallUpdate(read, status = Some(ToolCallStatus.Completed),
               content = Some(List(ToolCallContent.Content(ContentBlock.text("scalaVersion := \"3.7.2\"")))))))
        _ <- say(SessionUpdate.ToolCallStarted(ToolCall(ToolCallId("t2"), "grep", status = ToolCallStatus.InProgress,
               rawInput = Some(Json.Obj("pattern" -> Json.Str("x"))))))
        _ <- say(SessionUpdate.ToolCallUpdated(ToolCallUpdate(ToolCallId("t2"), status = Some(ToolCallStatus.Failed),
               rawOutput = Some(Json.Str("no such directory")))))
        _ <- say(SessionUpdate.AgentMessageChunk(ContentBlock.text("It sets ")))
        _ <- say(SessionUpdate.AgentMessageChunk(ContentBlock.text("3.7.2.")))
        _ <- say(SessionUpdate.UsageUpdate(1000, 200000, Some(Cost(0.01, "USD"))))
        _ <- say(SessionUpdate.UsageUpdate(1200, 200000, Some(Cost(0.02, "USD"))))
      yield PromptResponse(StopReason.EndTurn, Some(TurnUsage(1500, 100, 50, cachedReadTokens = Some(1000), cachedWriteTokens = Some(350))),
        Some(Json.Obj("agent" -> Json.Obj("steps" -> Json.Num(2)))))

  private def loop(seen: Ref[Seen], withModelOption: Boolean = true): AcpAgentLoop =
    AcpAgentLoop.connecting: (_, client, _) =>
      for
        (clientSide, agentSide) <- Transport.pipe
        _      <- AcpAgent.serve(agentSide)(c => ZIO.succeed(ScriptedAcpAgent(c, seen, withModelOption))).forkScoped
        agent  <- AcpClient.connect(clientSide, client)
      yield agent

  private val empty = Seen("", Nil, None, None, "", None)

  def spec = suite("AcpAgentLoop")(
    test("a run opens a session with its MCP servers, selects the model, and records the turn, usage and cost") {
      for
        seen   <- Ref.make(empty)
        result <- loop(seen).run("what scala version?", "large", List(McpServerConfig("docs", "http://localhost:1/mcp", Map("A" -> "b"))),
                    AgentPolicy.default)
        saw    <- seen.get
      yield assertTrue(
        saw.prompt == "what scala version?",
        saw.model.contains("large"),
        saw.mcpServers == List(McpServer.Http("docs", "http://localhost:1/mcp", List(HttpHeader("A", "b")))),
        saw.cwd.nonEmpty,
        saw.permission.contains(RequestPermissionOutcome.Selected(PermissionOptionId("yes"))),
        result.answer == "It sets 3.7.2.",
        result.toolCalls == 2,
        result.capturedToolCalls == List(CapturedToolCall("Read", """{"file_path":"build.sbt"}"""), CapturedToolCall("grep", """{"pattern":"x"}""")),
        result.events.drop(1) == List(
          TranscriptEvent.Thinking("Let me look."),
          TranscriptEvent.ToolCall("Read", """{"file_path":"build.sbt"}"""),
          TranscriptEvent.ToolResult("Read", "scalaVersion := \"3.7.2\"", isError = false),
          TranscriptEvent.ToolCall("grep", """{"pattern":"x"}"""),
          TranscriptEvent.ToolResult("grep", "no such directory", isError = true),
          TranscriptEvent.AgentMessage("It sets 3.7.2."),
        ),
        result.events.headOption.exists { case TranscriptEvent.Note(n) => n.contains("scripted 1.0") && n.contains("model large"); case _ => false },
        result.inputTokens == 1450L, result.outputTokens == 50L, result.cacheReadTokens == 1000L, result.cacheCreationTokens == 350L,
        result.costUsd == 0.02,
        result.sessionId.contains("s1"),
        result.meta.exists(_.toJson.contains("\"steps\":2")),
        result.toolCallTimings.map(t => (t.name, t.input, t.status)) ==
          List(("Read", """{"file_path":"build.sbt"}""", "completed"), ("grep", """{"pattern":"x"}""", "failed")),
      )
    },
    test("runIn passes ACP's own session parameters; an empty model id keeps the agent's") {
      for
        seen   <- Ref.make(empty)
        dir    <- ZIO.attemptBlocking(java.nio.file.Files.createTempDirectory("acp-spec"))
        stdio   = McpServer.Stdio("ide", "/bin/ide", List("stdio"), List(EnvVariable("PORT", "1")))
        meta    = Json.Obj("additionalRoots" -> Json.Arr(Json.Str("/skills")))
        result <- loop(seen).runIn("hi", "", AcpAgentLoop.Session(dir, List(stdio), meta = Some(meta)))
        saw    <- seen.get
      yield assertTrue(saw.model.isEmpty, saw.cwd == dir.toString, saw.mcpServers == List(stdio), saw.meta.contains(meta),
        result.events.headOption.exists { case TranscriptEvent.Note(n) => n.contains("model small"); case _ => false })
    },
    test("without a model config option the model is set with session/set_model, and the run fails if it can't be") {
      for
        seen  <- Ref.make(empty)
        error <- loop(seen, withModelOption = false).run("hi", "claude-x", Nil, AgentPolicy.default).flip
      yield assertTrue(error.getMessage == "ACP session/set_model failed: Method not found: session/set_model")
    },
    test("a model the agent's config option doesn't offer fails the run") {
      for
        seen  <- Ref.make(empty)
        error <- loop(seen).run("hi", "huge", Nil, AgentPolicy.default).flip
      yield assertTrue(error.getMessage.contains("has no model huge"), error.getMessage.contains("small, large"))
    },
  )
