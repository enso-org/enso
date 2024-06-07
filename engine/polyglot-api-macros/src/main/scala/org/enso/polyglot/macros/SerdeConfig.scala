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
      else {
        in.rollbackToken()
        val path = in.readString(null)
        new File(path)
      }
    }

    override def encodeValue(x: File, out: JsonWriter): Unit = {
      if (x == null) out.writeNull()
      else out.writeVal(x.getPath)
    }

    override def nullValue: File = null
  }

}
