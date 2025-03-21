package org.enso.logging.service.telemetry

import java.util

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

object ApiMessage {
  implicit val payloadCodec: JsonValueCodec[Payload] =
    JsonCodecMaker.make[Payload](CodecMakerConfig.withAllowRecursiveTypes(true))
  implicit val logCodec: JsonValueCodec[Log] =
    JsonCodecMaker.make[Log](CodecMakerConfig.withAllowRecursiveTypes(true))

  case class Payload(
    logs: util.List[Log]
  )

  case class Log(
    message: String,
    kind: String,
    metadata: util.Map[String, Object]
  )

  def serializePayload(
    payload: Payload
  ): String = {
    writeToString(payload)
  }
}
