package com.jamesward.zio_evals

import zio.*
import zio.test.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object SkillMaterializerSpec extends ZIOSpecDefault:

  private def deleteRecursively(path: Path): Unit =
    if Files.isDirectory(path) then
      val stream = Files.list(path)
      try stream.forEach(deleteRecursively)
      finally stream.close()
    Files.deleteIfExists(path)
    ()

  private def tempDir(prefix: String): ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory(prefix)))(p => ZIO.attempt(deleteRecursively(p)).ignore)

  def spec = suite("SkillMaterializer")(
    test("materializes an exact classpath SKILL.md") {
      ZIO.scoped {
        for
          root <- tempDir("skill-classpath")
          skills = AgentSkills.available(AgentSkill("fixture-skill", SkillSource.Classpath("skills/fixture-skill/SKILL.md")))
          out <- SkillMaterializer.materialize(skills, root)
          text <- ZIO.attempt(Files.readString(out.head.directory.resolve("SKILL.md"), StandardCharsets.UTF_8))
        yield assertTrue(out.map(_.name) == List("fixture-skill"), text.contains("Use domain types."))
      }
    },
    test("copies a directory skill and its supporting files") {
      ZIO.scoped {
        for
          source <- tempDir("skill-source")
          root <- tempDir("skill-directory")
          _ <- ZIO.attempt {
                 Files.writeString(source.resolve("SKILL.md"), "---\nname: local\n---\n")
                 Files.createDirectories(source.resolve("reference"))
                 Files.writeString(source.resolve("reference/details.md"), "details")
               }
          skills = AgentSkills.available(AgentSkill("local", SkillSource.Directory(source.toString)))
          out <- SkillMaterializer.materialize(skills, root)
          markerExists <- ZIO.attempt(Files.isRegularFile(out.head.directory.resolve("SKILL.md")))
          details <- ZIO.attempt(Files.readString(out.head.directory.resolve("reference/details.md")))
        yield assertTrue(markerExists, details == "details")
      }
    },
    test("rejects duplicate or unsafe skill names") {
      ZIO.scoped {
        for
          root <- tempDir("skill-invalid")
          one = AgentSkill("same", SkillSource.Classpath("skills/fixture-skill/SKILL.md"))
          duplicate <- SkillMaterializer.materialize(AgentSkills.available(one, one), root).exit
          unsafe <- SkillMaterializer.materialize(
                      AgentSkills.available(AgentSkill("../escape", SkillSource.Classpath("skills/fixture-skill/SKILL.md"))),
                      root,
                    ).exit
        yield assertTrue(duplicate.isFailure, unsafe.isFailure)
      }
    },
  )
