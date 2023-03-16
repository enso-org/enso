package org.enso.syntax.text

import org.enso.data.Span
import io.circe
import io.circe.Json
import io.circe.generic.auto._
import io.circe.parser._

import java.util.UUID

////////////////////////////////

class InternalError(reason: String, cause: Throwable = None.orNull)
    extends Exception(s"Internal error $reason", cause)

////////////////
//// Parser ////
////////////////

object SourceFile {
  val METATAG = "\n\n\n#### METADATA ####\n"
}

class Parser {
  import Parser._

  def splitMeta(code: String): (String, IDMap, Json) = {
    import SourceFile._
    code.split(METATAG) match {
      case Array(input) => (input, Seq(), Json.obj())
      case Array(input, rest) =>
        val meta = rest.split('\n')
        if (meta.length < 2) {
          throw new ParserError(s"Expected two lines after METADATA.")
        }
        val idmap = idMapFromJson(meta(0)).left.map { error =>
          throw new ParserError("Could not deserialize idmap.", error)
        }.merge
        val metadata = decode[Json](meta(1)).left.map { error =>
          throw new ParserError("Could not deserialize metadata.", error)
        }.merge
        (input, idmap, metadata)
      case arr: Array[_] =>
        throw new ParserError(
          s"Could not not deserialize metadata (found ${arr.length - 1} metadata sections)"
        )
    }
  }
}

object Parser {

  type IDMap = Seq[(Span, UUID)]

  def apply(): Parser = new Parser()

  def idMapFromJson(json: String): Either[circe.Error, IDMap] =
    decode[IDMap](json)

  //// Exceptions ////

  class ParserError(reason: String, cause: Throwable = None.orNull)
      extends InternalError(s"in parser $reason", cause)
}
