zio-evals
---------

[![javadocs.dev](https://www.javadocs.dev/com.jamesward/zio-evals_3/badge.svg)](https://www.javadocs.dev/com.jamesward/zio-evals_3/latest)

A ZIO 2 / Scala 3 toolkit for running **agent evals**: give an agent a task,
run it through several *arms* (different tool loadouts), grade the answers with
a judge, and compare. The engine is persistence-free and HTTP-free, so it works
both **inside an application** (persist results via an observer) and **as
integration tests** (assert on the returned results).

### Core concepts

- **`AgentLoop`** — the provider-agnostic seam: `run` / `runStructured` take a
  prompt, a model id, a list of `McpServerConfig` to expose, and an
  `AgentPolicy` (web / tool-search), and return an `AgentRunResult` (answer +
  efficiency metrics + a `TranscriptEvent` list). Bundled backends:
  - **`ClaudeCliAgentLoop`** — the `claude -p` CLI (stream-json parsed for full
    metrics + transcript; MCP via `--mcp-config`).
  - **`KiroCliAgentLoop`** — the `kiro-cli chat` CLI (headless; MCP via a
    throwaway agent config). Plain-text output, so metrics are limited to
    measured latency.
  A host can plug in its own backend (e.g. a hosted-agent runner) by
  implementing `AgentLoop`.

- **`EvalArm`** — one configuration under test: which `McpServerConfig`s to
  expose and the `AgentPolicy`. Helpers: `EvalArm.modelOnly` / `.web` / `.mcp`.

- **`McpServerConfig`** — an MCP server (`name`, `url`, `headers`) reached over
  HTTP; rendered into each CLI's config shape.

- **`EvalSpec`** — the model-facing definition: `task`, `criteria` (judge
  rubric), and deterministic `EvalCheck`s.

- **`Judge`** — grades all arms' answers together. `AgentLoopJudge` is the
  bundled default (an `AgentLoop` + the judge's own MCP servers, retry-once).

- **`EvalRunner`** — drives arms × models × samples through the `AgentLoop`,
  grades each sample, aggregates into `ArmResult`s, and streams live progress
  through an optional `EvalObserver`. No `DataSource`, no host types.

### Persistence

The library never persists anything. A host attaches its own ids/timestamps and
writes `ArmResult`s (and the `EvalTranscript` in each) via `EvalObserver` hooks
or from the returned list. `EvalCodecs.encode`/`decode` serialize the transcript
as JSON via the derived `Schema`.

### License

MIT
