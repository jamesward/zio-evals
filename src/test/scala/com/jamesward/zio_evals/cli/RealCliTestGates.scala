package com.jamesward.zio_evals
package cli

import zio.*
import zio.test.*

// Reusable gates for paid real-CLI tests in the regular test suite. A missing,
// unauthenticated, or non-working CLI makes the guarded test ignored (not
// failed), so `test` remains portable across CI and developer machines.
//
// Claude's gate performs a tiny real `claude -p` inference after confirming the
// executable exists. That is the definitive working/auth check regardless of
// whether auth comes from an environment variable or `claude auth login`.
// Kiro's `validate` checks both installation and `kiro-cli user whoami` (free,
// no model call).
object RealCliTestGates:

  private val claudeAvailable: UIO[Boolean] =
    (for
      installed <- ClaudeCliAgentLoop.isInstalled
      working   <- ClaudeCliAgentLoop.validate.isSuccess.when(installed).map(_.contains(true))
    yield installed && working)
      .catchAllCause(c => ZIO.logWarningCause("real Claude CLI test gate failed; test will be skipped", c).as(false))

  private val kiroAvailable: UIO[Boolean] =
    KiroCliAgentLoop.validate.isSuccess
      .catchAllCause(c => ZIO.logWarningCause("real kiro-cli test gate failed; test will be skipped", c).as(false))

  val ifClaudeAvailable: TestAspectPoly =
    new TestAspectPoly:
      def some[R, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
        spec.whenZIO(Live.live(claudeAvailable))

  val ifKiroAvailable: TestAspectPoly =
    new TestAspectPoly:
      def some[R, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
        spec.whenZIO(Live.live(kiroAvailable))
