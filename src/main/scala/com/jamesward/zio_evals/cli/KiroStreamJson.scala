package com.jamesward.zio_evals
package cli

import zio.schema.*
import zio.schema.annotation.fieldName

object KiroStreamJson:

  private final case class WireData(
      finalText: Option[String],
      status: Option[String],
      message: Option[String],
  ) derives Schema

  private final case class WireLine(
      @fieldName("type") kind: String,
      data: WireData,
  ) derives Schema

  def finalText(stdout: String): Either[String, String] =
    val lines = stdout.linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(line => EvalCodecs.decode[WireLine](line).toOption)
      .toList

    lines.reverse.collectFirst {
      case line if line.kind == "runFinished" && line.data.status.contains("success") =>
        line.data.finalText.filter(_.nonEmpty).toRight("kiro-cli runFinished contained no finalText")
      case line if line.kind == "runError" =>
        Left(line.data.message.getOrElse("kiro-cli reported runError"))
    }.getOrElse(Left("kiro-cli stream-json contained no runFinished/runError event"))
