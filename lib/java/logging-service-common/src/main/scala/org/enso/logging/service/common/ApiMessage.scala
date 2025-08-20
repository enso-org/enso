package org.enso.logging.service.common

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsScala}

object ApiMessage {
  private val TELEMETRY_KIND = "Telemetry"
  private val ENGINE_KIND    = "Engine"

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
        throw new IllegalStateException("Should not be used for decoding")
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
          case _                    => encodeString(obj.toString, out)
        }
      }

      private def encodeString(
        str: String,
        out: JsonWriter
      ): Unit = {
        if (str.equalsIgnoreCase("true")) {
          out.writeVal(true)
        } else if (str.equalsIgnoreCase("false")) {
          out.writeVal(false)
        } else {
          try {
            val l = java.lang.Long.parseLong(str)
            out.writeVal(l)
          } catch {
            case _: NumberFormatException =>
              try {
                val d = java.lang.Double.parseDouble(str)
                out.writeVal(d)
              } catch {
                case _: NumberFormatException =>
                  out.writeVal(str)
              }
          }
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

  def createTelemetryLog(
    message: String,
    metadata: java.util.Map[String, Object]
  ): Log = {
    Log(message, TELEMETRY_KIND, metadata.asScala.toMap)
  }

  def createEngineLog(
    message: String,
    args: java.util.Map[String, Object]
  ): Log = {
    Log(message, ENGINE_KIND, args.asScala.toMap)
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
