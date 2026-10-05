package com.jamesward.zio_evals

import zio.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

// Runs a host process with bounded waiting. Output goes to temp files rather
// than pipes, so large outputs (agent stream-json transcripts, build logs)
// can't deadlock the child, and the process is killed on timeout or fiber
// interruption.
object HostProcess:

  final case class Result(stdout: String, stderr: String, exitCode: Int, timedOut: Boolean)

  def run(
      argv:    List[String],
      timeout: Duration,
      stdin:   Option[Array[Byte]] = None,
      env:     Map[String, String] = Map.empty,
      cwd:     Option[Path] = None,
  ): Task[Result] =
    ZIO.scoped {
      for
        out  <- tempFile("out")
        err  <- tempFile("err")
        in   <- ZIO.foreach(stdin)(bytes => tempFile("in").tap(p => ZIO.attemptBlocking(Files.write(p, bytes))))
        proc <- ZIO.acquireRelease(
                  ZIO.attemptBlocking {
                    val pb = ProcessBuilder(argv*)
                    pb.redirectOutput(out.toFile)
                    pb.redirectError(err.toFile)
                    in match
                      case Some(p) => pb.redirectInput(p.toFile)
                      case None    => pb.redirectInput(ProcessBuilder.Redirect.from(java.io.File("/dev/null")))
                    cwd.foreach(d => pb.directory(d.toFile))
                    env.foreach((k, v) => pb.environment().put(k, v))
                    pb.start()
                  }
                )(p => ZIO.succeed { if p.isAlive then p.destroyForcibly(); () })
        finished <- ZIO.attemptBlockingCancelable(proc.waitFor(timeout.toMillis, TimeUnit.MILLISECONDS))(
                      ZIO.succeed { proc.destroyForcibly(); () }
                    )
        _        <- ZIO.attemptBlocking { proc.destroyForcibly(); proc.waitFor(10, TimeUnit.SECONDS) }.unless(finished)
        stdout   <- ZIO.attemptBlocking(Files.readString(out, StandardCharsets.UTF_8))
        stderr   <- ZIO.attemptBlocking(Files.readString(err, StandardCharsets.UTF_8))
      yield Result(stdout, stderr, if finished then proc.exitValue() else 124, timedOut = !finished)
    }

  private def tempFile(suffix: String): ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempFile("zio-evals-", s".$suffix")))(p =>
      ZIO.attemptBlocking(Files.deleteIfExists(p)).ignoreLogged
    )

// A bind mount from the host into a DockerSandbox container.
final case class DockerMount(host: String, container: String, readOnly: Boolean = false)

// A `Sandbox` that provisions one long-lived Docker container per workspace
// (`docker run -d ... sleep infinity`) and runs every command with
// `docker exec`. The container is force-removed when the workspace scope
// closes. The image supplies the system toolchain; nothing project-specific is
// expected to be present, so a run's dependency downloads and cold compiles
// happen inside the measured work.
//
//   * `defaultImage` — used when `WorkspaceSpec.image` is empty.
//   * `workdir`      — the workspace root inside the container; created and
//                      chowned to `user` on provision.
//   * `user`         — the user commands run as (`None` = the image default).
//   * `network`      — `--network` value (for example "host" so tests can
//                      reach ports published by sibling containers).
//   * `mounts`       — extra bind mounts (for example the Docker socket, which
//                      grants the workspace control of the host's Docker
//                      daemon: only use it with trusted images and code).
//   * `groupAdd`     — extra groups for `user` (for example the socket's gid).
//   * `env`          — environment for every command (merged under the
//                      workspace spec's `env`).
//   * `extraRunArgs` — any other `docker run` flags (resource limits, ...).
final case class DockerSandbox(
    defaultImage: String,
    workdir:      String = "/workspace",
    user:         Option[String] = None,
    network:      Option[String] = None,
    mounts:       List[DockerMount] = Nil,
    groupAdd:     List[String] = Nil,
    env:          Map[String, String] = Map.empty,
    extraRunArgs: List[String] = Nil,
    namePrefix:   String = "zio-evals",
    docker:       String = "docker",
) extends Sandbox:

  private val controlTimeout = 2.minutes

  private[zio_evals] def runArgs(name: String, spec: WorkspaceSpec): List[String] =
    List("run", "-d", "--name", name, "--entrypoint", "sleep") ++
      user.toList.flatMap(u => List("--user", u)) ++
      network.toList.flatMap(n => List("--network", n)) ++
      mounts.flatMap(m => List("-v", s"${m.host}:${m.container}${if m.readOnly then ":ro" else ""}")) ++
      groupAdd.flatMap(g => List("--group-add", g)) ++
      (env ++ spec.env).toList.sortBy(_._1).flatMap((k, v) => List("-e", s"$k=$v")) ++
      List("-w", workdir) ++
      extraRunArgs ++
      List(spec.image.getOrElse(defaultImage), "infinity")

  private def control(args: List[String]): IO[SandboxError, HostProcess.Result] =
    HostProcess.run(docker :: args, controlTimeout)
      .mapError(e => SandboxError.Unavailable(s"docker ${args.headOption.getOrElse("")} failed: ${e.getMessage}"))
      .flatMap { r =>
        if r.exitCode == 0 then ZIO.succeed(r)
        else ZIO.fail(SandboxError.ExecutionFailed(s"docker ${args.mkString(" ").take(300)} exited ${r.exitCode}: ${r.stderr.trim.take(2000)}"))
      }

  def provision(spec: WorkspaceSpec): ZIO[Scope, SandboxError, Workspace] =
    for
      suffix <- ZIO.succeed(java.util.UUID.randomUUID().toString.take(8))
      name    = s"$namePrefix-$suffix"
      _      <- ZIO.acquireRelease(control(runArgs(name, spec)))(_ =>
                  control(List("rm", "-f", name)).tapError(e => ZIO.logWarning(s"docker rm -f $name failed: $e")).ignore
                )
      // Create the workspace root and hand it to the workspace user, even when
      // the image did not create it.
      chown   = user.fold("")(u => s" && chown ${Workspace.shellQuote(u)} ${Workspace.shellQuote(workdir)}")
      _      <- control(List("exec", "--user", "0", name, "sh", "-c", s"mkdir -p ${Workspace.shellQuote(workdir)}$chown"))
      ws      = DockerWorkspace(this, name)
      _      <- ZIO.foreachDiscard(spec.seedFiles)((p, bytes) => ws.writeFile(p, bytes))
      _      <- ZIO.logInfo(s"DockerSandbox: provisioned $name from ${spec.image.getOrElse(defaultImage)}")
    yield ws

  private[zio_evals] def execArgs(container: String, argv: List[String], interactive: Boolean, env: Map[String, String]): List[String] =
    List("exec") ++ (if interactive then List("-i") else Nil) ++
      user.toList.flatMap(u => List("--user", u)) ++
      List("-w", workdir) ++
      env.toList.sortBy(_._1).flatMap((k, v) => List("-e", s"$k=$v")) ++
      (container :: argv)

  final class DockerWorkspace private[DockerSandbox] (sandbox: DockerSandbox, val container: String) extends Workspace:

    override def root: String = workdir

    private def resolve(path: String): String =
      if path.startsWith("/") then path else s"$workdir/$path"

    override def exec(
        argv:    List[String],
        timeout: Duration,
        stdin:   Option[Array[Byte]] = None,
        env:     Map[String, String] = Map.empty,
    ): IO[SandboxError, ExecResult] =
      // `timeout` inside the container stops the in-container process too, not
      // just the host-side `docker exec` client; the host-side bound is a
      // backstop with a grace period.
      val secs    = math.max(1L, timeout.toSeconds)
      val bounded = List("timeout", "-k", "10", secs.toString) ++ argv
      HostProcess.run(docker :: execArgs(container, bounded, stdin.nonEmpty, env), timeout.plus(30.seconds), stdin)
        .mapError(e => SandboxError.ExecutionFailed(s"docker exec failed: ${e.getMessage}"))
        .flatMap { r =>
          if r.timedOut then ZIO.fail(SandboxError.TimedOut(s"command exceeded $timeout: ${argv.mkString(" ").take(200)}"))
          else ZIO.succeed(ExecResult(r.stdout, r.stderr, r.exitCode))
        }

    def run(command: String, timeout: Duration): IO[SandboxError, ExecResult] =
      exec(List("sh", "-c", command), timeout)

    def writeFile(path: String, bytes: Array[Byte]): IO[SandboxError, Unit] =
      val target = resolve(path)
      exec(List("sh", "-c", """mkdir -p "$(dirname "$1")" && cat > "$1"""", "sh", target), controlTimeout, Some(bytes))
        .flatMap(r => ZIO.fail(SandboxError.ExecutionFailed(s"writeFile $target: ${r.stderr.trim}")).unless(r.exitCode == 0).unit)

    def readFile(path: String): IO[SandboxError, Array[Byte]] =
      // Base64 keeps binary content intact through the text-captured stdout.
      exec(List("base64", resolve(path)), controlTimeout).flatMap { r =>
        if r.exitCode == 0 then ZIO.attempt(java.util.Base64.getMimeDecoder.decode(r.stdout.trim)).orElseFail(SandboxError.ExecutionFailed(s"readFile $path: bad base64"))
        else ZIO.fail(SandboxError.ExecutionFailed(s"readFile $path: ${r.stderr.trim}"))
      }

    override def copyOut(path: String, dest: Path): IO[SandboxError, Unit] =
      ZIO.attemptBlocking(Files.createDirectories(dest)).orElseFail(SandboxError.ExecutionFailed(s"cannot create $dest")) *>
        sandbox.control(List("cp", s"$container:${resolve(path)}/.", dest.toAbsolutePath.toString)).unit

object DockerSandbox:
  // Builds `dockerfileDir` as `tag` unless an image with that tag already
  // exists. Callers put a content hash in the tag to rebuild on change.
  def ensureImage(
      tag:           String,
      dockerfileDir: Path,
      buildArgs:     Map[String, String] = Map.empty,
      docker:        String = "docker",
      timeout:       Duration = 30.minutes,
  ): Task[Unit] =
    HostProcess.run(List(docker, "image", "inspect", tag), 1.minute).flatMap { r =>
      if r.exitCode == 0 then ZIO.unit
      else
        val args = buildArgs.toList.sortBy(_._1).flatMap((k, v) => List("--build-arg", s"$k=$v"))
        ZIO.logInfo(s"building image $tag from $dockerfileDir") *>
          HostProcess.run(List(docker, "build", "-t", tag) ++ args ++ List(dockerfileDir.toAbsolutePath.toString), timeout).flatMap { b =>
            ZIO.fail(RuntimeException(s"docker build $tag failed (${b.exitCode}):\n${(b.stdout + b.stderr).takeRight(4000)}")).unless(b.exitCode == 0).unit
          }
    }

  def isAvailable: UIO[Boolean] =
    HostProcess.run(List("docker", "info", "--format", "{{.ServerVersion}}"), 30.seconds).map(_.exitCode == 0).orElseSucceed(false)
