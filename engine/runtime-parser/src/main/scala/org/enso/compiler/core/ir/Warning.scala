package org.enso.compiler.core.ir

import org.enso.compiler.core.{ir, IR}

/** A trait for all warnings in Enso's IR. */
trait Warning extends Diagnostic

object Warning {

  case class DuplicatedImport(
    override val location: Option[IdentifiedLocation],
    originalImport: ir.module.scope.Import,
    symbolName: String
  ) extends Warning {
    override def message(source: (IdentifiedLocation => String)): String = {
      val originalImportRepr =
        originalImport.location match {
          case Some(location) =>
            s"'${originalImport.showCode()}' in ${source(location)}"
          case None => originalImport.showCode()
        }
      s"Duplicated import of $symbolName. The original import is ${originalImportRepr}."
    }

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning about a `@Tail_Call` annotation placed in a non-tail
    * position.
    *
    * @param location the location of the annotated application
    */
  case class WrongTco(override val location: Option[IdentifiedLocation])
      extends Warning {
    override def message(source: (IdentifiedLocation => String)): String =
      "A @Tail_Call annotation was placed in a non-tail-call position."

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning about a `@Builtin_Method` annotation placed in a method
    * with unexpected body.
    *
    * @param location the location of the annotated application
    */
  case class WrongBuiltinMethod(
    override val location: Option[IdentifiedLocation]
  ) extends Warning {
    override def message(source: (IdentifiedLocation => String)): String =
      "A @Builtin_Method annotation allows only the name of the builtin node in the body."

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning raised when a method is defined with a `self` parameter defined
    * not in the first position in the parameters' list.`
    *
    * @param ir            the annotated application
    * @param paramPosition the reason why the annotation cannot be obeyed
    */
  case class WrongSelfParameterPos(
    funName: Name,
    ir: IR,
    paramPosition: Int
  ) extends Warning {
    override val location: Option[IdentifiedLocation] = ir.location

    override def message(source: (IdentifiedLocation => String)): String =
      s"${funName.name}: Self parameter should be declared as the first parameter. Instead its position is: ${paramPosition + 1}."

    override def diagnosticKeys(): Array[Any] =
      Array(ir.showCode(), paramPosition)
  }

  /** A warning raised when a call is annotated with `@Auto_Parallel`, but the
    * annotation cannot be obeyed.
    *
    * @param ir     the annotated application
    * @param reason the reason why the annotation cannot be obeyed
    */
  case class FailedParallelism(
    ir: IR,
    reason: String
  ) extends Warning {
    override val location: Option[IdentifiedLocation] = ir.location

    override def message(source: (IdentifiedLocation => String)): String =
      s"The expression ${ir.showCode()} could not be parallelised: $reason."

    override def diagnosticKeys(): Array[Any] = Array(ir.showCode(), reason)
  }

  case class NonUnitTypeUsedOnValueLevel(ir: Name, context: String)
      extends Warning {

    /** @return a human-readable description of this error condition.
      */
    override def message(source: (IdentifiedLocation => String)): String =
      s"A non-unit type ${ir.name} is used on value level (in ${context})." +
      " This is probably an error."

    /** The location at which the diagnostic occurs. */
    override val location: Option[IdentifiedLocation] = ir.location

    /** The important keys identifying identity of the diagnostic
      */
    override def diagnosticKeys(): Array[Any] = Array(ir.name)
  }

}
