package com.jamesward.zio_evals

import zio.test.*

object AgentSkillsSpec extends ZIOSpecDefault:

  private val zen = AgentSkill("zen-of-james", SkillSource.Classpath("zen/SKILL.md"))

  def spec = suite("AgentSkills")(
    test("available skills leave the task unchanged") {
      assertTrue(AgentSkills.available(zen).augmentPrompt("write code") == "write code")
    },
    test("explicit skills are invoked before the task") {
      val other = AgentSkill("other", SkillSource.Directory("/tmp/other"))
      assertTrue(AgentSkills.explicit(zen, other).augmentPrompt("write code") == "/zen-of-james /other write code")
    },
    test("empty explicit skills leave the task unchanged") {
      assertTrue(AgentSkills(Nil, SkillActivation.Explicit).augmentPrompt("write code") == "write code")
    },
  )
