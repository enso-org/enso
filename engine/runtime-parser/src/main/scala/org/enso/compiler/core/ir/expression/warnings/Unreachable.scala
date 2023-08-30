package org.enso.compiler.core.ir
package expression
package warnings

/** Warnings for unreachable code. */
sealed trait Unreachable extends Warning {
  val location: Option[IdentifiedLocation]
}

object Unreachable {

  /** A warning for unreachable branches in a case expression.
    *
    * @param location the location of the unreachable branches
    */
  sealed case class Branches(
    override val location: Option[IdentifiedLocation]
  ) extends Unreachable {
    val atLocation =
      if (location.isDefined) {
        s" at location ${location.get}"
      } else {
        ""
      }

    override def message: String = s"Unreachable case branches$atLocation."

    override def diagnosticKeys(): Array[Any] = Array(atLocation)
  }
}
