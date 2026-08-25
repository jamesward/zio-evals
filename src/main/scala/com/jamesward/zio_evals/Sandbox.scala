package com.jamesward.zio_evals

import zio.*

// The seam that lets action-based evals (`EvalCheck.CommandSucceeds`/
// `CommandOutputMatches`/`FileExists`) run real, untrusted, agent-produced
// commands in an isolated, ephemeral workspace instead of the host process.
// `Sandbox` is provided as a layer at the program edge so the provider is
// swappable and judge-only / transcript-check evals need none at all.

final case class WorkspaceSpec(seedFiles: Map[String, Array[Byte]] = Map.empty)

final case class ExecResult(stdout: String, stderr: String, exitCode: Int)

enum SandboxError derives CanEqual:
  case Unavailable(message: String)
  case ExecutionFailed(message: String)

trait Workspace:
  def writeFile(path: String, bytes: Array[Byte]): IO[SandboxError, Unit]
  def run(command: String, timeout: Duration): IO[SandboxError, ExecResult]
  def readFile(path: String): IO[SandboxError, Array[Byte]]

trait Sandbox:
  def provision(spec: WorkspaceSpec): ZIO[Scope, SandboxError, Workspace]

object Sandbox:
  def provision(spec: WorkspaceSpec): ZIO[Sandbox & Scope, SandboxError, Workspace] =
    ZIO.serviceWithZIO[Sandbox](_.provision(spec))

  // Default layer: no sandbox provider configured, so `Command*`/`FileExists`
  // checks fail honestly instead of silently no-op-ing. Judge-only /
  // transcript-check evals never call `provision` and are unaffected.
  val unavailable: ULayer[Sandbox] =
    ZLayer.succeed:
      new Sandbox:
        def provision(spec: WorkspaceSpec): ZIO[Scope, SandboxError, Workspace] =
          ZIO.fail(SandboxError.Unavailable("execution sandbox not configured"))
