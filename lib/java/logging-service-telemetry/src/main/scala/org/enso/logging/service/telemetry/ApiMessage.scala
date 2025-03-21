package org.enso.logging.service.telemetry

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsScala}

object ApiMessage {
  private val KIND = "Telemetry"

  implicit val payloadCodec: JsonValueCodec[Payload] =
    JsonCodecMaker.make[Payload](CodecMakerConfig.withAllowRecursiveTypes(true))

  implicit val logCodec: JsonValueCodec[Log] =
    JsonCodecMaker.make[Log](CodecMakerConfig.withAllowRecursiveTypes(true))

  implicit val objectCodec: JsonValueCodec[Any] =
    new JsonValueCodec[Any] {
      override def decodeValue(
        in: JsonReader,
        default: Any
      ): Any = {
        val str = in.readString("")
        if (str.equalsIgnoreCase("true")) {
          true
        } else if (str.equalsIgnoreCase("false")) {
          false
        } else {
          try {
            java.lang.Long.parseLong(str)
          } catch {
            case _: NumberFormatException =>
              try {
                java.lang.Double.parseDouble(str)
              } catch {
                case _: NumberFormatException => str
              }
          }
        }
      }

      override def encodeValue(
        obj: Any,
        out: JsonWriter
      ): Unit = {
        obj match {
          case i: Integer           => out.writeVal(i)
          case l: java.lang.Long    => out.writeVal(l)
          case d: java.lang.Double  => out.writeVal(d)
          case b: java.lang.Boolean => out.writeVal(b)
          case _                    => out.writeVal(obj.toString)
        }
      }

      override def nullValue: AnyRef = null
    }

  case class Payload(
    logs: List[Log]
  )

  case class Log(
    message: String,
    kind: String,
    metadata: Map[String, Any]
  )

  def createLog(
    message: String,
    metadata: java.util.Map[String, Object]
  ): Log = {
    Log(message, KIND, metadata.asScala.toMap)
  }

  def createPayload(
    logs: java.util.List[Log]
  ): Payload = {
    Payload(logs.asScala.toList)
  }

  def serializePayload(
    payload: Payload
  ): String = {
    writeToString(payload)
  }
}
