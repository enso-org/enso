package org.enso.compiler.core.ir
package expression
package errors

import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{randomId, Identifier, ToStringHelper}

import scala.annotation.unused

/** A representation of an Enso syntax error.
  *
  * @param at          the error location
  * @param reason      the cause of this error
  * @param passData    the pass metadata associated with this node
  * @param diagnostics compiler diagnostics for this node
  */
sealed case class Syntax(
  at: IdentifiedLocation,
  reason: Syntax.Reason,
  override val passData: MetadataStorage      = MetadataStorage(),
  override val diagnostics: DiagnosticStorage = DiagnosticStorage()
) extends Error
    with Diagnostic.Kind.Interactive
    with module.scope.Definition
    with module.scope.Export
    with module.scope.Import
    with IRKind.Primitive {
  override protected var id: Identifier = randomId

  /** Creates a copy of `this`.
    *
    * @param ast         the error location
    * @param reason      the cause of this error
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    * @param id          the identifier for the new node
    * @return a copy of `this`, updated with the specified values
    */
  def copy(
    at: IdentifiedLocation         = at,
    reason: Syntax.Reason          = reason,
    passData: MetadataStorage      = passData,
    diagnostics: DiagnosticStorage = diagnostics,
    id: Identifier                 = id
  ): Syntax = {
    val res = Syntax(at, reason, passData, diagnostics)
    res.id = id
    res
  }

  /** @inheritdoc */
  override def duplicate(
    @unused keepLocations: Boolean = true,
    keepMetadata: Boolean          = true,
    keepDiagnostics: Boolean       = true,
    keepIdentifiers: Boolean       = false
  ): Syntax =
    copy(
      passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
      diagnostics =
        if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
      id = if (keepIdentifiers) id else randomId
    )

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Syntax =
    this

  /** @inheritdoc */
  override val location: Option[IdentifiedLocation] = Option(at)

  /** @inheritdoc */
  override def mapExpressions(fn: Expression => Expression): Syntax = this

  /** @inheritdoc */
  override def toString: String =
    s"""
       |Error.Syntax(
       |at = $at,
       |reason = $reason,
       |location = $location,
       |passData = ${this.showPassData},
       |diagnostics = $diagnostics,
       |id = $id
       |)
       |""".toSingleLine

  /** @inheritdoc */
  override def children: List[IR] = List()

  /** @inheritdoc */
  override def message: String = reason.explanation

  /** @inheritdoc */
  override def formattedMessage: String = s"${message}."

  override def diagnosticKeys(): Array[Any] = Array(reason)

  /** @inheritdoc */
  override def showCode(indent: Int): String = "Syntax_Error"
}

object Syntax {

  /** A common type for all syntax errors expected by the language.
    */
  sealed trait Reason {

    /** @return a human-readable description of the error.
      */
    def explanation: String
  }

  case object SuspendedArgInAtom extends Reason {
    override def explanation: String =
      "Atoms may not have suspended arguments"
  }

  case class InvalidEscapeSequence(lit: String) extends Reason {
    override def explanation: String = s"Invalid escape sequence $lit"
  }

  case object InvalidBaseInDecimalLiteral extends Reason {
    override def explanation: String =
      "Cannot change base of the fractional part of a number literal"
  }

  case class InvalidBase(base: String) extends Reason {
    override def explanation: String =
      s"$base is not a valid numeric base"
  }

  case class InvalidNumberForBase(base: String, number: String) extends Reason {
    override def explanation: String =
      s"$number is not valid in $base"
  }

  case class UnsupportedSyntax(syntaxName: String) extends Reason {
    override def explanation: String =
      s"Syntax is not supported yet: $syntaxName"
  }

  case object InvalidUnderscore extends Reason {
    override def explanation: String =
      s"Invalid use of _"
  }

  case object InvalidPattern extends Reason {
    override def explanation: String =
      s"Cannot define a pattern outside a pattern context"
  }

  case class InvalidImport(
    message: String = "Imports must have a valid module path"
  ) extends Reason {
    override def explanation: String =
      s"Invalid Import: $message"
  }

  case class InvalidExport(
    message: String = "Exports must have a valid module path"
  ) extends Reason {
    override def explanation: String =
      s"Invalid Export: $message"
  }

  case object InvalidStandaloneSignature extends Reason {
    override def explanation: String =
      s"Invalid stand-alone signature expression"
  }

  case class MethodDefinedInline(methodName: String) extends Reason {
    override def explanation: String =
      s"Cannot define $methodName, methods are not supported in the " +
      s"inline flow"
  }

  case object UnexpectedDeclarationInType extends Reason {
    override def explanation: String =
      "Unexpected declaration in the body of a type"
  }

  case object InvalidTypeDefinition extends Reason {
    override def explanation: String =
      "Invalid definition of a type"
  }

  case class TypeDefinedInline(typeName: String) extends Reason {
    override def explanation: String =
      s"Cannot define $typeName, type definitions are not supported " +
      s"in the inline flow"
  }

  case object EmptyParentheses extends Reason {
    override def explanation: String =
      "Parentheses can't be empty"
  }

  case object UnexpectedExpression extends Reason {
    override def explanation: String = "Unexpected expression"
  }

  case object AmbiguousExpression extends Reason {
    override def explanation: String = "Ambiguous expression"
  }

  case object InvalidSelfArgUsage extends Reason {
    override def explanation: String =
      "Self argument cannot be used in static methods"
  }

  case object UnrecognizedToken extends Reason {
    override def explanation: String = "Unrecognized token"
  }

  case object InvalidSuffix extends Reason {
    override def explanation: String = "Invalid suffix"
  }

  case object UnclosedTextLiteral extends Reason {
    override def explanation: String = "Unclosed text literal"
  }

  case object NamedArgInSection extends Reason {
    override def explanation: String = "Named argument in operator section"
  }

  case object NamedArgInOperator extends Reason {
    override def explanation: String = "Named argument in operator section"
  }

  case object InvalidOperatorName extends Reason {
    override def explanation: String = "Invalid operator name"
  }

  case class InvalidForeignDefinition(details: String) extends Reason {
    override def explanation: String =
      s"Invalid foreign definition. $details"
  }
}
