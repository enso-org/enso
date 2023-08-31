package org.enso.compiler.core.ir

import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.core.IR.{randomId, Identifier, ToStringHelper}

/** Enso literals. */
sealed trait Literal extends Expression with IRKind.Primitive {

  /** @inheritdoc */
  override def mapExpressions(fn: Expression => Expression): Literal

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Literal

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Literal
}

object Literal {

  /** A numeric Enso literal.
    *
    * @param base        the optional base for the number, expressed in decimal
    * @param value       the textual representation of the numeric literal
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Number(
    base: Option[String],
    value: String,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Literal {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param base        the optional base for the number, expressed in decimal
      * @param value       the textual representation of the numeric literal
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      base: Option[String]                 = base,
      value: String                        = value,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: Identifier                       = id
    ): Number = {
      val res = Number(base, value, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Number =
      copy(
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Number =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Number = this

    /** @inheritdoc */
    override def toString: String =
      s"""Literal.Number(
         |base = $base,
         |value = $value,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = if (this.base.isDefined) {
      s"${base.get}_$value"
    } else value

    /** Checks whether the literal represents a fractional value.
      *
      * @return `true` if the value is fractional, `false` otherwise.
      */
    def isFractional: Boolean = value.contains(".")

    /** Checks the values in the literal converts that to approviate JVM value.
      *
      * @return Double, Long, BigInteger
      */
    @throws[CompilerError]
    def numericValue: Any = {
      if (isFractional) {
        value.toDouble
      } else if (base.isDefined) {
        val baseNum =
          try {
            Integer.parseInt(base.get)
          } catch {
            case _: NumberFormatException =>
              throw new CompilerError(
                s"Invalid number base $base seen during codegen."
              )
          }
        try {
          val longVal = java.lang.Long.parseLong(value, baseNum)
          longVal
        } catch {
          case _: NumberFormatException =>
            try {
              new java.math.BigInteger(value, baseNum)
            } catch {
              case _: NumberFormatException =>
                throw new CompilerError(
                  s"Invalid number base $base seen during codegen."
                )
            }
        }
      } else {
        value.toLongOption.getOrElse(new java.math.BigInteger(value))
      }
    }
  }

  /** A textual Enso literal.
    *
    * @param text        the text of the literal
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Text(
    text: String,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Literal {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param text        the text of the literal
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      text: String                         = text,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: Identifier                       = id
    ): Text = {
      val res = Text(text, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Text =
      copy(
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Text =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Text = this

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Literal.String(
         |text = $text,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = s""""$text""""
  }
}
