package org.enso.syntax.text

import cats.Monoid

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

object Location {
  implicit val optionSpanMonoid: Monoid[Option[Location]] =
    new Monoid[Option[Location]] {
      def empty: Option[Location] = None

      def combine(
        x: Option[Location],
        y: Option[Location]
      ): Option[Location] =
        x match {
          case None => y
          case Some(lSpan @ Location(lStart, _)) =>
            y match {
              case None => Some(lSpan)
              case Some(Location(_, rEnd)) =>
                Some(Location(lStart, rEnd))
            }
        }
    }
}
