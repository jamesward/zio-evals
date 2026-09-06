package com.jamesward.zio_evals

import zio.*

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.Using

final case class MaterializedSkill(name: String, directory: Path)

object SkillMaterializer:

  private val validName = "[A-Za-z0-9_-]+".r

  // Materialize every skill as <root>/<name>/SKILL.md. Directory skills retain
  // supporting files recursively; classpath skills currently copy the exact
  // SKILL.md resource named by SkillSource.Classpath.
  def materialize(
      skills:      AgentSkills,
      root:        Path,
      classLoader: ClassLoader = Thread.currentThread().getContextClassLoader,
  ): Task[List[MaterializedSkill]] =
    val duplicateNames = skills.values.groupBy(_.name).collect { case (name, xs) if xs.size > 1 => name }.toList.sorted
    if duplicateNames.nonEmpty then
      ZIO.fail(IllegalArgumentException(s"duplicate agent skill names: ${duplicateNames.mkString(", ")}"))
    else
      ZIO.foreach(skills.values)(materializeOne(_, root, classLoader))

  private def materializeOne(skill: AgentSkill, root: Path, classLoader: ClassLoader): Task[MaterializedSkill] =
    for
      _ <- ZIO.fail(IllegalArgumentException(s"invalid agent skill name '${skill.name}'"))
             .unless(validName.matches(skill.name))
      target <- ZIO.attemptBlocking {
                  val dir = root.resolve(skill.name).normalize()
                  Files.createDirectories(dir)
                  dir
                }
      _ <- skill.source match
             case SkillSource.Directory(path) => copyDirectory(Paths.get(path), target)
             case SkillSource.Classpath(resource) => copyClasspathSkill(resource, target, classLoader)
      marker = target.resolve("SKILL.md")
      _ <- ZIO.fail(IllegalArgumentException(s"agent skill '${skill.name}' has no SKILL.md"))
             .unlessZIO(ZIO.attemptBlocking(Files.isRegularFile(marker)))
    yield MaterializedSkill(skill.name, target)

  private def copyDirectory(source: Path, target: Path): Task[Unit] =
    ZIO.attemptBlocking {
      if !Files.isDirectory(source) then
        throw IllegalArgumentException(s"agent skill directory does not exist: $source")

      Using.resource(Files.walk(source)) { paths =>
        paths.iterator().asScala.foreach { path =>
          val relative = source.relativize(path)
          val dest     = target.resolve(relative).normalize()
          if !dest.startsWith(target) then
            throw IllegalArgumentException(s"agent skill path escapes target directory: $path")
          if Files.isDirectory(path) then Files.createDirectories(dest)
          else
            Option(dest.getParent).foreach(Files.createDirectories(_))
            Files.copy(path, dest, StandardCopyOption.REPLACE_EXISTING)
        }
      }
    }

  private def copyClasspathSkill(resource: String, target: Path, classLoader: ClassLoader): Task[Unit] =
    ZIO.attemptBlocking {
      val normalized = resource.stripPrefix("/")
      val input = Option(classLoader.getResourceAsStream(normalized))
        .getOrElse(throw IllegalArgumentException(s"agent skill classpath resource not found: $normalized"))
      Using.resource(input) { stream =>
        Files.copy(stream, target.resolve("SKILL.md"), StandardCopyOption.REPLACE_EXISTING)
      }
    }
