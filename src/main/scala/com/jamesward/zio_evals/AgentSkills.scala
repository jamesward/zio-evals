package com.jamesward.zio_evals

import zio.schema.*

// Where an agent skill comes from. A Directory is the skill directory itself
// (the directory containing SKILL.md). A Classpath source names one exact
// SKILL.md resource, which makes a SkillsJar usable without an sbt extraction
// plugin for single-file skills.
enum SkillSource derives CanEqual, Schema:
  case Directory(path: String)
  case Classpath(resource: String)

final case class AgentSkill(name: String, source: SkillSource) derives CanEqual, Schema

enum SkillActivation derives CanEqual, Schema:
  // Advertise the skill to the model and let the model invoke it when relevant.
  case Available
  // Invoke every configured skill at the start of the one-shot prompt. This is
  // useful for an eval that measures skill-content efficacy independently of
  // description/automatic-trigger quality.
  case Explicit

final case class AgentSkills(
    values:     List[AgentSkill] = Nil,
    activation: SkillActivation = SkillActivation.Available,
) derives CanEqual, Schema:

  def isEmpty: Boolean = values.isEmpty
  def nonEmpty: Boolean = values.nonEmpty

  // Claude supports stacked slash-command skill invocation in a one-shot
  // prompt. Kiro's headless initial-input parser accepts built-in commands only,
  // so its backend implements Explicit by loading SKILL.md as a file resource.
  private[zio_evals] def augmentPrompt(prompt: String): String =
    activation match
      case SkillActivation.Available => prompt
      case SkillActivation.Explicit if values.isEmpty => prompt
      case SkillActivation.Explicit =>
        s"${values.map(s => s"/${s.name}").mkString(" ")} $prompt"

object AgentSkills:
  val none: AgentSkills = AgentSkills()

  def available(skills: AgentSkill*): AgentSkills =
    AgentSkills(skills.toList, SkillActivation.Available)

  def explicit(skills: AgentSkill*): AgentSkills =
    AgentSkills(skills.toList, SkillActivation.Explicit)
