package org.enso.interpreter.test

import java.util.UUID
import org.enso.text.editing.model

private case class Item(start: Int, len: Int, id: UUID) {
  def toJsonString: String =
    s"""[{"index": {"value": $start}, "size": {"value": $len}}, "$id"]"""
}

/** A helper class for decorating source code with expression IDs.
  */
class Metadata(val prelude: String = "") {

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
    items ::= Item(prelude.length + start, len, id)
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
    s"$prelude$code\n\n\n#### METADATA ####\n$toJsonString\n[]"

  /** Checks whether given UUID is assigned to expected string
    * @param uuid the UUID to search for; defined by {@code #addItem}
    * @param code whole code to search in
    * @param expected the text that should be assigned to the UUID
    */
  def assertInCode(uuid: UUID, code: String, expected: String): Unit = {
    val full = prelude + code
    for (item <- items) {
      if (item.id == uuid) {
        val real = full.substring(item.start, item.start + item.len)
        if (real != expected) {
          throw new AssertionError(
            "Expecting\n`" + expected + "`\nbut found\n'" + real + "'"
          )
        }
        return
      }
    }
    throw new AssertionError("UUID " + uuid + " not found")
  }

  /** Verifies given line/column based position range contains
    * requested text and if so, it returns a range representing
    * those positions.
    */
  def assertInCode(
    code: String,
    start: model.Position,
    end: model.Position,
    expected: String
  ): model.Range = {
    val full = prelude + code
    if (start.line != end.line) {
      throw new AssertionError("Supporting only same line right now")
    }
    val actual = full.lines
      .toList()
      .get(start.line)
      .substring(start.character, end.character)
    val range = model.Range(start, end)
    if (actual != expected) {
      throw new AssertionError(s"Unexpected text at $range: $actual")
    }
    range
  }
}
