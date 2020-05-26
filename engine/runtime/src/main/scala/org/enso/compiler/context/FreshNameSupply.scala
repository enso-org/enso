package org.enso.compiler.context

import org.enso.compiler.core.IR

/** This class provides a supply of fresh names guaranteed not to exist in this
  * program.
  */
class FreshNameSupply {
  private var counter: Long = 0

  /** Creates a new name instance using the provided number.
    *
    * @param numId the numeric identifier to use in the name
    * @return a new name
    */
  private def mkName(numId: Long): IR.Name.Literal =
    IR.Name.Literal(s"<internal-${numId}>", None)

  /** Generates a name guaranteed not to exist in this program.
    *
    * @return a new name
    */
  def newName(): IR.Name.Literal = {
    val num = counter
    counter += 1

    mkName(num)
  }
}
