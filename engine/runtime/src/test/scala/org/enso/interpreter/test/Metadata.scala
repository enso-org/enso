package org.enso.interpreter.test

import java.util.UUID

private case class Item(start: Int, len: Int, id: UUID) {
  def toJsonString: String =
    s"""[{"index": {"value": $start}, "size": {"value": $len}}, "$id"]"""
}

/** A helper class for decorating source code with expression IDs.
  */
class Metadata {

  private var items: List[Item] = List()

  /** Adds another entry to this metadata container.
    *
    * @param start the start position of the entry.
    * @param len the length of the entry.
    * @param suggestion optional hexadecimal suggestion of UUID prefix
    * @return the new entry's id.
    */
  def addItem(start: Int, len: Int, suggestion: String = null): UUID = {
    var id = UUID.randomUUID();
    if (suggestion != null) {
      val lo = java.lang.Long.parseUnsignedLong(suggestion, 16);
      val hi = id.getMostSignificantBits();
      id = new UUID(lo, hi)
    }
    items ::= Item(start, len, id)
    id
  }

  private def toJsonString: String =
    "[" + items.map(_.toJsonString).mkString(",") + "]"

  /** Appends a serialized version of this at the end of a source code string.
    *
    * @param code the code to append metadata to.
    * @return the code decorated with this metadata.
    */
  def appendToCode(code: String): String =
    s"$code\n\n\n#### METADATA ####\n$toJsonString\n[]"
}
