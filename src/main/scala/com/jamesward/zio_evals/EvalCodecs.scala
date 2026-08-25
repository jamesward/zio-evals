package com.jamesward.zio_evals

import zio.schema.Schema
import zio.schema.codec.JsonCodec.jsonCodec

// Schema-derived JSON encode/decode for the eval domain types (notably
// `EvalTranscript`), so a host can persist a transcript as a JSON string
// without hand-rolling a codec. Uses the same schema-based codec everywhere so
// the on-disk shape is stable and matches the derived `Schema`.
object EvalCodecs:

  def encode[A](a: A)(using schema: Schema[A]): String =
    jsonCodec(schema).encodeJson(a, None).toString

  def decode[A](s: String)(using schema: Schema[A]): Either[String, A] =
    jsonCodec(schema).decodeJson(s)
