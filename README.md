zio-evals
---------

[![javadocs.dev](https://www.javadocs.dev/com.jamesward/zio-evals_3/badge.svg?1)](https://www.javadocs.dev/com.jamesward/zio-evals_3/latest)

A ZIO 2 / Scala 3 toolkit for running **agent evals**: give an agent a task,
run it through several *arms* (different tool loadouts), grade the answers with
a judge, and compare. The orchestration engine is persistence-free and
host-agnostic; network and process I/O belong to the selected agent, judge, and
sandbox implementations. It works both **inside an application** (persist
results via an observer) and **as integration tests** (assert on the returned
results).

### Core concepts

- **`AgentLoop`** — the provider-agnostic seam: `run` / `runStructured` take a
  prompt, a model id, a list of `McpServerConfig` to expose, an
  `AgentPolicy` (web / tool-search), and optionally `AgentSkills`; they return
  an `AgentRunResult` (answer + efficiency metrics + a `TranscriptEvent` list).
  Existing custom backends remain compatible for skill-free arms. Bundled backends:
  - **`ClaudeCliAgentLoop`** — the `claude -p` CLI (stream-json parsed for full
    metrics + transcript; MCP via `--mcp-config`).
  - **`KiroCliAgentLoop`** — the `kiro-cli chat` CLI (headless v2
    `stream-json`; MCP via a throwaway agent config). The final answer is
    lossless, while metrics are currently limited to measured latency.
  A host can plug in its own backend (e.g. a hosted-agent runner) by
  implementing `AgentLoop`.

- **`EvalArm`** — one configuration under test: which `McpServerConfig`s and
  `AgentSkills` to expose and the `AgentPolicy`. Helpers:
  `EvalArm.modelOnly` / `.web` / `.mcp` / `.withSkills`.

- **`AgentSkills`** — skills isolated to one arm. `SkillSource.Directory`
  recursively copies a skill directory; `SkillSource.Classpath` resolves an
  exact `SKILL.md` resource from a SkillsJar. `Available` activation measures
  automatic discovery, while `Explicit` invokes the configured skills before
  the task.

- **`McpServerConfig`** — an MCP server (`name`, `url`, `headers`) reached over
  HTTP; rendered into each CLI's config shape.

- **`EvalSpec`** — the model-facing definition: `task`, `criteria` (judge
  rubric), and deterministic `EvalCheck`s.

- **`Judge`** — grades all arms' answers together. `AgentLoopJudge` is the
  bundled generative default (an `AgentLoop` + the judge's own MCP servers,
  retry-once). `JevJudge` uses TypeSafe AI's Jev model for typed binary grading:
  it returns a calibrated pass probability per candidate and applies a
  configurable threshold.

- **`EvalRunner`** — drives arms × models × samples through the `AgentLoop`,
  grades each sample, aggregates into `ArmResult`s, and streams live progress
  through an optional `EvalObserver`. No `DataSource`, no host types.

### Example

Run one task through two arms — the agent alone vs. the agent with an MCP
server — grade them with a judge, and assert on the results:

```scala
import com.jamesward.zio_evals.*
import com.jamesward.zio_evals.cli.KiroCliAgentLoop
import zio.*

object MyEval extends ZIOAppDefault:
  val spec = EvalSpec(
    task     = "What is the capital of France?",
    criteria = "The answer must name Paris.",
    checks   = List(EvalCheck.AnswerContains("Paris")),
  )

  val arms = List(
    EvalArm.modelOnly(),
    EvalArm.mcp("atlas", "Agent with Atlas", List(McpServerConfig("atlas", "http://localhost:8080/mcp"))),
  )

  def run =
    val agentLoop = KiroCliAgentLoop()
    val judge     = AgentLoopJudge(agentLoop, judgeModelId = "claude-opus-4.8")
    for
      results <- EvalRunner.run(spec, arms, modelIds = List("claude-opus-4.8"), samples = 1, agentLoop, judge)
      _       <- ZIO.foreachDiscard(results)(r => Console.printLine(s"${r.arm.label}: ${r.verdict} (pass=${r.passRate})"))
    yield ()
```

To use Jev instead of a generative judge, configure `TYPESAFE_API_KEY` (and,
optionally, `TYPESAFE_DEFAULT_MODEL`; the default is `jev-latest`) and provide
TypeSafe AI's HTTP-backed client layer. Jev performs one typed binary
classification per candidate and includes the calibrated probability in each
rationale:

```scala
import com.jamesward.zio_typesafe_ai.TypeSafeAI
import zio.http.Client

val judged = (for
  judge   <- JevJudge.make(passThreshold = 0.5)
  results <- EvalRunner.run(spec, arms, List("claude-opus-4.8"), 1, agentLoop, judge)
yield results).provide(Client.default, TypeSafeAI.Client.live)
```

A skill can be loaded directly from a normal runtime SkillsJar dependency:

```scala
val zen = AgentSkill(
  "zen-of-james",
  SkillSource.Classpath("META-INF/skills/jamesward/skills/zen-of-james/SKILL.md"),
)

val arms = List(
  EvalArm.modelOnly("a", "A"),
  EvalArm.withSkills("b", "B", AgentSkills.explicit(zen)),
)
```

Both bundled CLI backends stage skills in a fresh temporary project. Kiro uses
always-loaded `file://` resources for `Explicit` activation and progressive
`skill://` resources for `Available`, while disabling inherited default
resources. Claude uses project-only settings and grants only the configured
skill names. Baseline and judge calls therefore do not inherit user-global
skills.

Each `ArmResult` carries the `verdict`, `passRate`, `checksPassed`, averaged
`metrics`, and the full per-sample transcript — so the same code works as an
integration test (assert on the list) or inside an app (persist via an
`EvalObserver`).

### License

MIT
