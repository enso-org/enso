package org.enso.syntax.text

////////////////////////////////////////////////////////////////////////////////
//// AbsolutePosition //////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/** Represents an expression's absolute positioning in a source file.
  * @param start the inclusive, 0-indexed position of the beginning
  *              of the expression
  * @param end the exclusive, 0-indexed position of the end of
  *            the expression
  */
case class Location(start: Int, end: Int) {
  def length: Int = end - start
}
