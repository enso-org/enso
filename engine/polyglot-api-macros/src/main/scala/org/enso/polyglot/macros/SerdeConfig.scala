package org.enso.polyglot.macros

import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import com.github.plokhotnyuk.jsoniter_scala.core.{
  JsonReader,
  JsonValueCodec,
  JsonWriter
}

import java.io.File
object SerdeConfig {

  /** Custom configuration for generating jsoniter's codecs.
    *
    * API data structures are recursive and have to be allowed explicitly.
    * `skipNestedOptionValues` has to be enabled to workaround an apparent
    * problem with nested option values present next to a single level option
    * values (reported under https://github.com/plokhotnyuk/jsoniter-scala/issues/1146).
    */
  val config = CodecMakerConfig
    .withAllowRecursiveTypes(allowRecursiveTypes = true)
    .withRequireCollectionFields(requireCollectionFields = true)
    .withTransientEmpty(false)
    .withSkipNestedOptionValues(true)

  implicit val fileCodec: JsonValueCodec[File] = new JsonValueCodec[File] {
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
