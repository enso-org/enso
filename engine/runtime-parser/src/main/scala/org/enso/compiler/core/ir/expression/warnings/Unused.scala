package org.enso.compiler.core.ir
package expression
package warnings

/** Warnings about unused language entities. */
sealed trait Unused extends Warning {
  val name: Name
}

object Unused {

  /** A warning about an unused function argument.
    *
    * @param name the name that is unused
    */
  sealed case class FunctionArgument(override val name: Name) extends Unused {
    override def message: String = s"Unused function argument ${name.name}."

    override def diagnosticKeys(): Array[Any] = Array(name.name)

    override def toString: String = s"Unused.FunctionArgument(${name.name})"

    override val location: Option[IdentifiedLocation] = name.location
  }

  sealed case class PatternBinding(override val name: Name) extends Unused {
    override def message: String = s"Unused pattern binding ${name.name}."

    override def diagnosticKeys(): Array[Any] = Array(name.name)

    override def toString: String = s"Unused.PatternBinding(${name.name})"

    override val location: Option[IdentifiedLocation] = name.location
  }

  /** A warning about an unused binding.
    *
    * @param name the name that is unused
    */
  sealed case class Binding(override val name: Name) extends Unused {
    override def message: String = s"Unused variable ${name.name}."

    override def diagnosticKeys(): Array[Any] = Array(name.name)

    override def toString: String = s"Unused.Binding(${name.name})"

    override val location: Option[IdentifiedLocation] = name.location
  }
}
