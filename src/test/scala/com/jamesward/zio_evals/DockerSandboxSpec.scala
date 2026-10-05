package com.jamesward.zio_evals

import zio.*
import zio.test.*

import java.nio.charset.StandardCharsets
import java.nio.file.Files

// Exercises a real container. Ignored (not failed) when no Docker daemon is
// reachable, so `test` stays portable.
object DockerSandboxSpec extends ZIOSpecDefault:

  private val image = "alpine:3.20"

  private val ifDocker: TestAspectPoly =
    new TestAspectPoly:
      def some[R, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
        spec.whenZIO(Live.live(DockerSandbox.isAvailable))

  private def utf8(b: Array[Byte]) = String(b, StandardCharsets.UTF_8)

  def spec = suite("DockerSandbox")(
    test("runArgs pins the image, user, network, mounts, groups and env") {
      val sb = DockerSandbox(image, user = Some("1000"), network = Some("host"),
        mounts = List(DockerMount("/var/run/docker.sock", "/var/run/docker.sock")), groupAdd = List("131"), env = Map("A" -> "1"))
      val args = sb.runArgs("n", WorkspaceSpec(image = Some("other:1"), env = Map("B" -> "2")))
      assertTrue(
        args.containsSlice(List("--user", "1000")),
        args.containsSlice(List("--network", "host")),
        args.containsSlice(List("-v", "/var/run/docker.sock:/var/run/docker.sock")),
        args.containsSlice(List("--group-add", "131")),
        args.containsSlice(List("-e", "A=1")) && args.containsSlice(List("-e", "B=2")),
        args.takeRight(2) == List("other:1", "infinity"),
      )
    },
    test("seeds, writes, reads, runs, execs with stdin/env, and copies out") {
      ZIO.scoped {
        for
          ws   <- DockerSandbox(image, namePrefix = "zio-evals-test")
                    .provision(WorkspaceSpec(seedFiles = Map("seed/a.txt" -> "seeded".getBytes)))
                    .mapError(_.toThrowable)
          seed <- ws.readFile("seed/a.txt").mapError(_.toThrowable)
          bin   = Array[Byte](0, 1, 2, -1, 10, 13)
          _    <- ws.writeFile("deep/dir/b.bin", bin).mapError(_.toThrowable)
          back <- ws.readFile("deep/dir/b.bin").mapError(_.toThrowable)
          pwd  <- ws.run("pwd && ls deep/dir", 30.seconds).mapError(_.toThrowable)
          fail <- ws.run("exit 3", 30.seconds).mapError(_.toThrowable)
          cat  <- ws.exec(List("sh", "-c", "cat; echo $X"), 30.seconds, Some("in".getBytes), Map("X" -> "y z")).mapError(_.toThrowable)
          tout <- ws.run("sleep 30", 2.seconds).either
          dest <- ZIO.attempt(Files.createTempDirectory("copy-out"))
          _    <- ws.copyOut(".", dest).mapError(_.toThrowable)
          copied <- ZIO.attempt(Files.readString(dest.resolve("seed/a.txt")))
        yield assertTrue(
          utf8(seed) == "seeded",
          back.sameElements(bin),
          pwd.stdout.linesIterator.toList == List("/workspace", "b.bin"),
          fail.exitCode == 3,
          cat.stdout == "iny z\n",
          tout.isLeft || tout.exists(_.exitCode != 0),
          copied == "seeded",
        )
      }
    },
  ) @@ ifDocker @@ TestAspect.timeout(5.minutes)
