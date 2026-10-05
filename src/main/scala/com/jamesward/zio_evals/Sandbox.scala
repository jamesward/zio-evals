package com.jamesward.zio_evals

import zio.*

import java.nio.file.Path

// The seam that lets action-based evals (`EvalCheck.CommandSucceeds`/
// `CommandOutputMatches`/`FileExists`) and workspace agents run real,
// untrusted, agent-produced commands in an isolated, ephemeral workspace
// instead of the host process. `Sandbox` is provided at the program edge so the
// provider is swappable and judge-only / transcript-check evals need none.
//
//   * `seedFiles` — files written (relative to the workspace root) before use.
//   * `image`     — the environment to provision (a container image for
//                   `DockerSandbox`); `None` selects the provider default.
//   * `env`       — environment variables visible to every command.
final case class WorkspaceSpec(
    seedFiles: Map[String, Array[Byte]] = Map.empty,
    image:     Option[String] = None,
    env:       Map[String, String] = Map.empty,
)

final case class ExecResult(stdout: String, stderr: String, exitCode: Int)

enum SandboxError derives CanEqual:
  case Unavailable(message: String)
  case ExecutionFailed(message: String)
  case TimedOut(message: String)

object SandboxError:
  extension (e: SandboxError)
    def message: String = e match
      case Unavailable(m)     => m
      case ExecutionFailed(m) => m
      case TimedOut(m)        => m
    def toThrowable: Throwable = RuntimeException(e.message)

trait Workspace:
  // The absolute workspace root inside the sandbox (the working directory of
  // `run` / `exec`, and the base for relative paths).
  def root: String = "."

  def writeFile(path: String, bytes: Array[Byte]): IO[SandboxError, Unit]
  // Runs a shell command line in the workspace root.
  def run(command: String, timeout: Duration): IO[SandboxError, ExecResult]
  def readFile(path: String): IO[SandboxError, Array[Byte]]

  // Runs an argv directly (no shell) with optional stdin and extra environment.
  // The default quotes the argv into `run`, so existing implementations keep
  // working; providers override it to avoid the shell and support stdin/env.
  def exec(
      argv:    List[String],
      timeout: Duration,
      stdin:   Option[Array[Byte]] = None,
      env:     Map[String, String] = Map.empty,
  ): IO[SandboxError, ExecResult] =
    if stdin.nonEmpty then ZIO.fail(SandboxError.Unavailable("this workspace does not support stdin"))
    else
      val exports = env.toList.map((k, v) => s"$k=${Workspace.shellQuote(v)}")
      run((exports ++ argv.map(Workspace.shellQuote)).mkString(" "), timeout)

  // Copies the workspace directory `path` (relative to `root`) into the host
  // directory `dest`, so a run's work product can be kept after the sandbox is
  // released.
  def copyOut(path: String, dest: Path): IO[SandboxError, Unit] =
    ZIO.fail(SandboxError.Unavailable("this workspace does not support copyOut"))

object Workspace:
  def shellQuote(s: String): String =
    if s.nonEmpty && s.forall(c => c.isLetterOrDigit || "-_./=:@%+,".contains(c)) then s
    else "'" + s.replace("'", "'\"'\"'") + "'"

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
