package org.enso.pkg

import io.circe._

/** A collection of utility codecs used in the [[Config]]. */
object ConfigCodecs {

  /** Get the decoded entity name.
    *
    * The entity name is encoded as a key of JSON object.
    *
    * {{{
    *   { `name`: entity }
    * }}}
    *
    * @param cursor the current focus in the JSON document
    */
  def getNameFromKey(cursor: ACursor): Option[String] =
    cursor.keys.flatMap {
      case keys if keys.size == 1 => keys.headOption
      case _                      => None
    }

  /** Get the scalar value of the provided JSON element.
    *
    * @param cursor the current focus in the JSON document
    */
  def getScalar(cursor: HCursor): Option[String] =
    cursor.value.fold(
      jsonNull    = None,
      jsonBoolean = value => Some(value.toString),
      jsonNumber  = value => Some(value.toString),
      jsonString  = value => Some(value),
      jsonArray   = _ => None,
      jsonObject  = _ => None
    )

}
