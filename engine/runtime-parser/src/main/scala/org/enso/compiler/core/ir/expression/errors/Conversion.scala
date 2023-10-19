package org.enso.compiler.core.ir
package expression
package errors

import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{randomId, Identifier}

/** An error resulting from processing conversion methods.
  *
  * @param storedIr    the IR that contains the error
  * @param reason      the explanation for the error
  * @param passData    the pass metadata associated with this node
  * @param diagnostics compiler dianostics for this node
  */
sealed case class Conversion(
  storedIr: IR,
  reason: Conversion.Reason,
  override val passData: MetadataStorage      = MetadataStorage(),
  override val diagnostics: DiagnosticStorage = DiagnosticStorage()
) extends Error
    with Diagnostic.Kind.Interactive
    with IRKind.Primitive
    with Name {
  override val name: String = "conversion_error"

  override def mapExpressions(fn: Expression => Expression): Conversion =
    this

  override def setLocation(
    location: Option[IdentifiedLocation]
  ): Conversion = {
    copy(storedIr = storedIr.setLocation(location))
  }

  /** Create a copy of `this`.
    *
    * @param storedIr    the IR that contains the error
    * @param reason      the explanation for the error
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler dianostics for this node
    * @param id          the identifier for the new node
    * @return a copy of `this`, updated with the specified values
    */
  def copy(
    storedIr: IR                   = storedIr,
    reason: Conversion.Reason      = reason,
    passData: MetadataStorage      = passData,
    diagnostics: DiagnosticStorage = diagnostics,
    id: Identifier                 = id
  ): Conversion = {
    val res = Conversion(storedIr, reason, passData, diagnostics)
    res.id = id
    res
  }

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean,
    keepMetadata: Boolean,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Conversion = {
    copy(
      storedIr = storedIr.duplicate(
        keepLocations,
        keepMetadata,
        keepDiagnostics,
        keepIdentifiers
      ),
      passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
      diagnostics =
        if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
      id = if (keepIdentifiers) id else randomId
    )
  }

  /** @inheritdoc */
  override def children: List[IR] = List(storedIr)

  /** @inheritdoc */
  override protected var id: Identifier = randomId

  /** @inheritdoc */
  override def showCode(indent: Int): String =
    s"(Error: ${storedIr.showCode(indent)})"

  /** @inheritdoc */
  override def message(source: Source): String = reason.explain

  override def diagnosticKeys(): Array[Any] = Array(reason.explain)

  /** @inheritdoc */
  override val location: Option[IdentifiedLocation] = storedIr.location
}

object Conversion {

  /** The reason for the error. */
  sealed trait Reason {
    def explain: String
  }

  case object MissingArgs extends Reason {
    override def explain: String =
      "A conversion definition must have at least one argument."
  }

  case object UnsupportedSourceType extends Reason {
    override def explain: String =
      "Arbitrary expressions are not yet supported as source types."
  }

  case class MissingSourceType(argName: String) extends Reason {
    override def explain: String =
      s"The argument `$argName` does not define a source type."
  }

  case class MissingSelfParam(argName: String) extends Reason {
    override def explain: String =
      s"""|Conversion definition must have an explicit `self` parameter in the first position.
          |Got `$argName` instead.""".stripMargin
  }

  case class NonDefaultedArgument(argName: String) extends Reason {
    override def explain: String =
      s"Additional arguments in a conversion must have a default, but " +
      s"`$argName` does not."
  }

  case class SuspendedSourceArgument(argName: String) extends Reason {
    override def explain: String =
      s"The `that` type argument in a conversion (here $argName) cannot " +
      s"be suspended."
  }

  case class InvalidSourceArgumentName(argName: String) extends Reason {
    override def explain: String =
      s"The source type argument must be ignored or named `that`, but" +
      s" ${argName} was found."
  }
}
