package com.jamesward.zio_evals
package cli

import zio.test.*

object KiroStreamJsonSpec extends ZIOSpecDefault:

  def spec = suite("KiroStreamJson")(
    test("extracts lossless final text from runFinished") {
      val stdout =
        """{"type":"runStarted","data":{"payloadSchema":"acp","engine":"v2"}}
          |{"type":"sessionUpdate","data":{"update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"ignored"}}}}
          |{"type":"runFinished","data":{"status":"success","finalText":"case _ => left *> right","finalTextTruncated":false}}
          |""".stripMargin
      assertTrue(KiroStreamJson.finalText(stdout) == Right("case _ => left *> right"))
    },
    test("surfaces a runError") {
      val stdout = """{"type":"runError","data":{"stage":"engine","message":"bad engine"}}"""
      assertTrue(KiroStreamJson.finalText(stdout) == Left("bad engine"))
    },
    test("fails when the stream has no terminal event") {
      val stdout = """{"type":"metadata","data":{"sessionId":"s"}}"""
      assertTrue(KiroStreamJson.finalText(stdout).isLeft)
    },
  )
