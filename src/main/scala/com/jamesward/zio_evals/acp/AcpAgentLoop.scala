package com.jamesward.zio_evals
package acp

import com.jamesward.zioacp.{AcpClient, AgentCommand, AgentConnection, Client}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.{Files, Path}

// An `AgentLoop` backed by any Agent Client Protocol agent (https://agentclientprotocol.com): zio-evals is the ACP
// client, as an editor would be. Each run launches the agent (`command`), initializes it, opens a session in a working
// directory with the run's MCP servers, selects the model, sends the prompt, and records the turn: the answer (agent
// message chunks), thinking, every tool call with its input and result, and the turn's usage when the agent reports
// it (ACP's `usage` on the prompt response; the cumulative `usage_update` cost). Permission requests are allowed (the
// first allow option): run it only where the agent may act. The agent process ends with the run.
//
// The agent's own tools are the agent's: ACP gives a client no say over them, so `AgentPolicy` doesn't apply, and
// skills and system prompts aren't supported. `runIn` runs a session with ACP's own session parameters (its working
// directory, MCP servers of any transport, additional directories and `_meta`), for hosts that drive an agent exactly
// as a particular editor does.
final class AcpAgentLoop private (
    connect:    AcpAgentLoop.Connect,
    initialize: InitializeRequest,
    runTimeout: Duration,
) extends AgentLoop:
  import AcpAgentLoop.*

  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    ZIO.scoped:
      ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("acp-eval")))(dir => ZIO.attemptBlocking(deleteRecursively(dir)).ignoreLogged)
        .flatMap(dir => runIn(prompt, modelId, Session(dir, mcpServers.map(httpServer))))

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    run(s"$prompt\n\nAnswer only with JSON matching this JSON Schema:\n${schema.toJson}", modelId, mcpServers, policy)
      .map(result => EvalJudging.sliceJson(result.answer))

  // One session with `session`'s parameters: `prompt`, with `modelId` selected first (unless empty).
  def runIn(prompt: String, modelId: String, session: Session): Task[AgentRunResult] =
    ZIO.scoped:
      for
        updates   <- Ref.make(Vector.empty[(Long, SessionUpdate)])
        stderr    <- Ref.make(Vector.empty[String])
        started   <- Clock.nanoTime
        agent     <- connect(session.cwd, recording(updates), line => stderr.update(lines => (lines :+ line).takeRight(50)))
        // a failure before the turn says what the agent wrote to stderr (why it exited, say)
        withStderr = (e: Throwable) => stderr.get.map(lines =>
                       if lines.isEmpty then e else RuntimeException(s"${e.getMessage}; agent stderr (last lines): ${lines.takeRight(10).mkString(" | ")}"))
        init      <- agent.initialize(initialize).mapError(rpc("initialize")).flatMapError(withStderr)
        created   <- agent.newSession(NewSessionRequest(session.cwd.toString, session.mcpServers, session.additionalDirectories, session.meta))
                       .mapError(rpc("session/new")).flatMapError(withStderr)
        model     <- select(agent, created, modelId)
        ready     <- Clock.nanoTime
        response  <- agent.prompt(PromptRequest(created.sessionId, List(ContentBlock.text(prompt)), meta = session.promptMeta))
                       .mapError(rpc("session/prompt"))
                       .timeoutFail(RuntimeException(s"the ACP agent didn't finish the turn within $runTimeout"))(runTimeout)
                       .tapError(_ => stderr.get.flatMap(lines => ZIO.logWarning(s"ACP agent stderr (last lines): ${lines.mkString("\n")}")))
        finished  <- Clock.nanoTime
        _         <- agent.closeSession(CloseSessionRequest(created.sessionId)).ignore
                       .when(init.agentCapabilities.sessionCapabilities.close.isDefined)
        seen      <- updates.get
      yield result(seen.map(_._2), response, (finished - ready) / 1000000L, (ready - started) / 1000000L, created.sessionId, init, model)
        .copy(toolCallTimings = timings(seen, ready, finished))

  // A model is selected through the session's model config option (the one whose id is `model`, else the one of the
  // `model` category: an agent may put something else in its model picker, as soness does its decision models), or else ACP's unstable `session/set_model`
  // (Kiro's agent offers models that way); an agent that takes neither runs with its default, noted in the events.
  private def select(agent: AgentConnection, created: NewSessionResponse, modelId: String): Task[String] =
    val wanted = modelId.trim
    val selects = created.configOptions.toList.flatten.collect { case select: SessionConfigOption.Select => select }
    val option = selects.find(_.id.value == "model").orElse(selects.find(_.category.contains(SessionConfigOptionCategory.Model)))
    if wanted.isEmpty then ZIO.succeed(option.fold("(agent default)")(_.currentValue.value))
    else option match
      case Some(select) =>
        ZIO.fail(IllegalArgumentException(s"the agent has no model $wanted; it offers ${select.options.all.map(_.value.value).mkString(", ")}"))
          .unless(select.options.all.exists(_.value.value == wanted)) *>
          agent.setSessionConfigOption(SetSessionConfigOptionRequest(created.sessionId, select.id,
            SessionConfigValue.ValueId(SessionConfigValueId(wanted)))).mapError(rpc("session/set_config_option")).as(wanted)
      case None =>
        agent.extMethod("session/set_model", Json.Obj("sessionId" -> Json.Str(created.sessionId.value), "modelId" -> Json.Str(wanted)))
          .mapError(rpc("session/set_model")).as(wanted)

object AcpAgentLoop:

  // ACP session parameters for one run: the session's working directory, its MCP servers, more workspace roots, the
  // `session/new` and `session/prompt` `_meta`.
  final case class Session(
      cwd:                   Path,
      mcpServers:            List[McpServer] = Nil,
      additionalDirectories: List[String] = Nil,
      meta:                  Option[Json.Obj] = None,
      promptMeta:            Option[Json.Obj] = None,
  )

  // A client with no file-system or terminal capabilities: the agent uses its own tools.
  val defaultInitialize: InitializeRequest =
    InitializeRequest(ProtocolVersion.V1, ClientCapabilities(), Some(Implementation("zio-evals", "0")))

  // How a run reaches its agent, for the session's working directory: the client to serve it, and where its stderr goes.
  type Connect = (Path, Client, String => UIO[Unit]) => ZIO[Scope, Throwable, AgentConnection]

  // The agent launched as `command` for each run, in the run's working directory unless `command` sets one.
  def apply(command: AgentCommand, initialize: InitializeRequest = defaultInitialize, runTimeout: Duration = 10.minutes): AcpAgentLoop =
    new AcpAgentLoop(
      (cwd, client, stderr) => AcpClient.launch(command.copy(cwd = command.cwd.orElse(Some(cwd))), client, stderr),
      initialize,
      runTimeout,
    )

  // An agent reached some other way (in-process, over a socket) for each run.
  def connecting(connect: Connect, initialize: InitializeRequest = defaultInitialize, runTimeout: Duration = 10.minutes): AcpAgentLoop =
    new AcpAgentLoop(connect, initialize, runTimeout)

  def httpServer(server: McpServerConfig): McpServer =
    McpServer.Http(server.name, server.url, server.headers.toList.map((name, value) => HttpHeader(name, value)))

  private def rpc(method: String)(error: RpcError): Throwable = RuntimeException(s"ACP $method failed: ${error.message}")

  private def recording(updates: Ref[Vector[(Long, SessionUpdate)]]): Client =
    new Client:
      def sessionUpdate(notification: SessionNotification): UIO[Unit] =
        Clock.nanoTime.flatMap(now => updates.update(_ :+ (now -> notification.update)))
      def requestPermission(request: RequestPermissionRequest): IO[RpcError, RequestPermissionResponse] =
        val allow = request.options.find(o => o.kind == PermissionOptionKind.AllowOnce || o.kind == PermissionOptionKind.AllowAlways)
        ZIO.succeed(RequestPermissionResponse(allow.fold(RequestPermissionOutcome.Cancelled)(o => RequestPermissionOutcome.Selected(o.optionId))))

  // A tool call as its updates describe it so far.
  private final case class Call(name: String, input: Option[Json], status: Option[ToolCallStatus], output: Option[String])

  // The run's result from the turn's updates and response. Message and thought chunks are joined into one event per
  // run of chunks; each tool call is one call event (with its input as last reported) and, once it ended, one result.
  private[acp] def result(
      updates: Vector[SessionUpdate],
      response: PromptResponse,
      latencyMs: Long,
      startupMs: Long,
      sessionId: SessionId,
      init: InitializeResponse,
      model: String,
  ): AgentRunResult =
    val calls = updates.foldLeft(Map.empty[String, Call]):
      case (known, SessionUpdate.ToolCallStarted(c)) =>
        known.updated(c.toolCallId.value, Call(c.name.getOrElse(c.title), c.rawInput, Some(c.status), output(c.rawOutput, c.content)))
      case (known, SessionUpdate.ToolCallUpdated(u)) =>
        val before = known.getOrElse(u.toolCallId.value, Call(u.name.orElse(u.title).getOrElse("?"), None, None, None))
        known.updated(u.toolCallId.value, Call(
          before.name, u.rawInput.orElse(before.input), u.status.orElse(before.status),
          output(u.rawOutput, u.content.getOrElse(Nil)).orElse(before.output),
        ))
      case (known, _) => known
    def callEvent(id: String): TranscriptEvent =
      val call = calls(id)
      TranscriptEvent.ToolCall(call.name, call.input.fold("{}")(_.toJson))
    val (events, _) = updates.foldLeft((Vector.empty[TranscriptEvent], Set.empty[String])):
      case ((out, started), SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _)) => out.lastOption match
        case Some(TranscriptEvent.AgentMessage(text)) => (out.init :+ TranscriptEvent.AgentMessage(text + t), started)
        case _                                        => (out :+ TranscriptEvent.AgentMessage(t), started)
      case ((out, started), SessionUpdate.AgentThoughtChunk(ContentBlock.Text(t, _, _), _, _)) => out.lastOption match
        case Some(TranscriptEvent.Thinking(text)) => (out.init :+ TranscriptEvent.Thinking(text + t), started)
        case _                                    => (out :+ TranscriptEvent.Thinking(t), started)
      case ((out, started), SessionUpdate.ToolCallStarted(c)) if !started(c.toolCallId.value) =>
        (out :+ callEvent(c.toolCallId.value), started + c.toolCallId.value)
      case ((out, started), SessionUpdate.ToolCallUpdated(u)) if u.status.exists(isDone) =>
        val id = u.toolCallId.value
        val call = calls(id)
        val withCall = if started(id) then out else out :+ callEvent(id)
        (withCall :+ TranscriptEvent.ToolResult(call.name, call.output.getOrElse(""), call.status.contains(ToolCallStatus.Failed)), started + id)
      case (state, _) => state
    val answer = updates.collect { case SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _) => t }.mkString
    val cost = updates.collect { case SessionUpdate.UsageUpdate(_, _, Some(cost), _) if cost.currency == "USD" => cost.amount }.lastOption
    val usage = response.usage
    val agentInfo = init.agentInfo.fold("an ACP agent")(i => s"${i.title.getOrElse(i.name)} ${i.version}")
    AgentRunResult(
      answer,
      iterations = 0,
      toolCalls = calls.size,
      // ACP's inputTokens leave out the cached categories, which zio-evals counts in
      inputTokens = usage.fold(0L)(u => u.inputTokens + u.cachedReadTokens.getOrElse(0L) + u.cachedWriteTokens.getOrElse(0L)),
      outputTokens = usage.fold(0L)(_.outputTokens),
      latencyMs = latencyMs,
      events = TranscriptEvent.Note(s"$agentInfo, model $model, session ${sessionId.value}; started in $startupMs ms; " +
        s"stop reason ${response.stopReason.toJson}") +: events.toList,
      cacheReadTokens = usage.flatMap(_.cachedReadTokens).getOrElse(0L),
      cacheCreationTokens = usage.flatMap(_.cachedWriteTokens).getOrElse(0L),
      costUsd = cost.getOrElse(0.0),
      sessionId = Some(sessionId.value),
      meta = response.meta,
    )

  // Each tool call's span from its first update to the update that ended it (or the turn's end), relative to `start`.
  private[acp] def timings(updates: Vector[(Long, SessionUpdate)], start: Long, end: Long): List[ToolCallTiming] =
    def id(u: SessionUpdate): Option[String] = u match
      case SessionUpdate.ToolCallStarted(c) => Some(c.toolCallId.value)
      case SessionUpdate.ToolCallUpdated(c) => Some(c.toolCallId.value)
      case _                                => None
    val byId = updates.flatMap((t, u) => id(u).map(_ -> (t, u))).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    val order = updates.flatMap((_, u) => id(u)).distinct
    order.toList.map: callId =>
      val events = byId(callId)
      val first = events.head._1
      val done = events.collectFirst(Function.unlift {
        case (t, SessionUpdate.ToolCallUpdated(u)) => u.status.filter(isDone).map(t -> _)
        case _                                     => None
      })
      val name = events.map(_._2).flatMap {
        case SessionUpdate.ToolCallStarted(c) => Some(c.name.getOrElse(c.title))
        case SessionUpdate.ToolCallUpdated(c) => c.name.orElse(c.title)
        case _                                => None
      }.headOption.getOrElse("?")
      val input = events.map(_._2).flatMap {
        case SessionUpdate.ToolCallStarted(c) => c.rawInput
        case SessionUpdate.ToolCallUpdated(c) => c.rawInput
        case _                                => None
      }.lastOption.fold("{}")(_.toJson)
      val status = done.fold("unfinished")(d => if d._2 == ToolCallStatus.Failed then "failed" else "completed")
      val meta = events.map(_._2).flatMap { case SessionUpdate.ToolCallStarted(c) => c.meta; case _ => None }.headOption
      ToolCallTiming(name, input, status, (first - start).max(0) / 1000000L, (done.fold(end)(_._1) - first).max(0) / 1000000L, meta)

  private def isDone(status: ToolCallStatus): Boolean = status == ToolCallStatus.Completed || status == ToolCallStatus.Failed

  // A tool call's output: its raw output (a string as is), or the text of its content.
  private def output(raw: Option[Json], content: List[ToolCallContent]): Option[String] =
    raw.map(j => j.asString.getOrElse(j.toJson)).orElse:
      val text = content.collect { case ToolCallContent.Content(ContentBlock.Text(t, _, _), _) => t }
      Option.when(text.nonEmpty)(text.mkString("\n"))

  private def deleteRecursively(path: Path): Unit =
    if Files.isDirectory(path) then Files.list(path).toList.forEach(deleteRecursively)
    Files.deleteIfExists(path)
    ()
