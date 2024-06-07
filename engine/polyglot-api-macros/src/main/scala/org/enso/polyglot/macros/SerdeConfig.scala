package org.enso.polyglot.macros

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

import java.io.File
object SerdeConfig {

  /**
    * Custom configuration for generating jsoniter's codecs.
    *
    * API data structures are recursive and have to be allowed explicitly.
    * `skipNestedOptionValues` has to be enabled to workaround an apparent
    */
  val config = CodecMakerConfig
    .withAllowRecursiveTypes(allowRecursiveTypes = true)
    .withRequireCollectionFields(requireCollectionFields = true)
    .withTransientEmpty(false)
    .withSkipNestedOptionValues(true)

  implicit lazy val fileCodec: JsonValueCodec[File] = new JsonValueCodec[File] {
    override def decodeValue(in: JsonReader, default: File): File = {
      val t = in.nextToken()

      if (t == 'n') in.readNullOrError(null, "expected 'null' or JSON value")
      else if (t == '"') {
        in.rollbackToken()
        val path = in.readString(null)
        if (path == null) null
        else new File(path)
      } else if (t == '{') {
        if (!in.isNextToken('}')) {
          in.rollbackToken()
          val key = in.readKeyAsString()
          if (key != "file") {
            throw new RuntimeException("invalid field name, expected `file` got `" + key + "`")
          }
          val path = in.readString(null)
          if (!in.isNextToken('}')) {
            in.objectEndOrCommaError()
          }
          new File(path)
        } else {
          null
        }

      } else throw new RuntimeException("Invalid value, cannot deserialize at " + t)
    }

    override def encodeValue(x: File, out: JsonWriter): Unit = {
      out.writeObjectStart()
      if (x == null) out.writeNull()
      else {
        out.writeKey("file")
        out.writeVal(x.getPath)
      }
      out.writeObjectEnd()
    }

    override def nullValue: File = null
  }

}
