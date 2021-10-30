package org.enso.compiler.context

import org.enso.compiler.core.IR

/** This class provides a supply of fresh names guaranteed not to exist in this
  * program.
  */
class FreshNameSupply {
  private var counter: Long = 0

  private def mkName(
    numId: Long,
    isReferent: Boolean,
    isMethod: Boolean
  ): IR.Name.Literal = {
    val refMarker = if (isReferent) "ref" else ""
    IR.Name.Literal(
      s"<internal-$refMarker-${numId}>",
      isReferent,
      isMethod,
      None
    )
  }

  /** Generates a name guaranteed not to exist in this program.
    *
    * @param isReferent whether or not the name should be marked as referent.
    * @param isMethod whether or not the name should represent a method name.
    * @return a new name
    */
  def newName(
    isReferent: Boolean = false,
    isMethod: Boolean   = false
  ): IR.Name.Literal = {
    val num = counter
    counter += 1

    mkName(num, isReferent, isMethod)
  }
}
