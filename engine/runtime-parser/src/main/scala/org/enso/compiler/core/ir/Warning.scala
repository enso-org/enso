package org.enso.compiler.core.ir

import org.enso.compiler.core.{ir, IR}

/** A trait for all warnings in Enso's IR. */
trait Warning extends Diagnostic

object Warning {

  case class DuplicatedImport(
    override val identifiedLocation: IdentifiedLocation,
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
    * @param identifiedLocation the location of the annotated application
    */
  case class WrongTco(override val identifiedLocation: IdentifiedLocation)
      extends Warning {
    override def message(source: (IdentifiedLocation => String)): String =
      "A @Tail_Call annotation was placed in a non-tail-call position."

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning about an invocation of a value that is not a function.
    *
    * This warning indicates a place that will result in a Not_Invokable error in runtime.
    *
    * @param identifiedLocation the location of the call
    * @param typeRepresentation the type of the value that was called
    */
  case class NotInvokable(
    override val identifiedLocation: IdentifiedLocation,
    typeRepresentation: String
  ) extends Warning {
    override def message(source: (IdentifiedLocation => String)): String =
      s"Invoking a value that has a non-function type $typeRepresentation will result in a Not_Invokable error in runtime."

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning indicating a mismatch between a type expected by an expression and the type that is provided.
    *
    * Currently, this warning is only raised if the mismatch is guaranteed to happen - i.e. running the expression will
    * always result in a runtime Type_Error.
    *
    * @param identifiedLocation the location of the type assertion
    * @param expectedType the type that was expected in the assertion
    * @param actualType the type that was provided
    */
  case class TypeMismatch(
    override val identifiedLocation: IdentifiedLocation,
    expectedType: String,
    actualType: String
  ) extends Warning {
    override def message(source: (IdentifiedLocation => String)): String =
      s"Got an expression of type $actualType that will never match $expectedType. This will always result in a Type_Error in runtime."

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning about calling a method (or field getter) that is not defined on the given type.
    *
    * This warning indicates a place that will result in a No_Such_Method error in runtime.
    *
    * @param identifiedLocation the location of the call
    * @param methodDescription the description of the method
    */
  case class NoSuchMethod(
    override val identifiedLocation: IdentifiedLocation,
    methodDescription: String
  ) extends Warning {
    override def message(source: (IdentifiedLocation => String)): String = {
      s"Calling $methodDescription will result in a No_Such_Method error in runtime."
    }

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning about a `@Builtin_Method` annotation placed in a method
    * with unexpected body.
    *
    * @param identifiedLocation the location of the annotated application
    */
  case class WrongBuiltinMethod(
    override val identifiedLocation: IdentifiedLocation
  ) extends Warning {
    override def message(source: (IdentifiedLocation => String)): String =
      "A @Builtin_Method annotation allows only the name of the builtin node in the body."

    override def diagnosticKeys(): Array[Any] = Array()
  }

  /** A warning raised when a method is defined with a `self` parameter defined
    * not in the first position in the parameters' list.
    *
    * @param ir            the annotated application
    * @param paramPosition the reason why the annotation cannot be obeyed
    */
  case class WrongSelfParameterPos(
    funName: Name,
    ir: IR,
    paramPosition: Int
  ) extends Warning {
    override def identifiedLocation: IdentifiedLocation =
      ir.identifiedLocation()

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
    override def identifiedLocation: IdentifiedLocation =
      ir.identifiedLocation()

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
    override def identifiedLocation: IdentifiedLocation =
      ir.identifiedLocation()

    /** The important keys identifying identity of the diagnostic
      */
    override def diagnosticKeys(): Array[Any] = Array(ir.name)
  }

  case class Syntax(ir: IR, message: String) extends Warning {

    /** @return a human-readable description of this error condition.
      */
    override def message(source: (IdentifiedLocation => String)): String =
      message

    /** The location at which the diagnostic occurs. */
    override def identifiedLocation: IdentifiedLocation =
      ir.identifiedLocation()

    /** The important keys identifying identity of the diagnostic
      */
    override def diagnosticKeys(): Array[Any] = Array()
  }

}
