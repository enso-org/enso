package org.enso.compiler.core.ir

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.IR
import org.enso.compiler.core.Identifier
import org.enso.compiler.core.IR.{indentLevel, mkIndent, randomId}

import java.util.UUID
import scala.jdk.FunctionConverters.enrichAsScalaFromFunction

trait Expression extends IR {

  /** Performs a recursive traversal of the IR, potentially transforming it.
    *
    * @param fn the function to apply across the IR
    * @return the IR, potentially transformed
    */
  def transformExpressions(
    fn: PartialFunction[Expression, Expression]
  ): Expression = {
    if (fn.isDefinedAt(this)) {
      fn(this)
    } else {
      mapExpressions(_.transformExpressions(fn))
    }
  }

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Expression

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Expression

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Expression
}

object Expression {

  // TODO Remove suspended blocks from Enso.

  /** A block expression.
    *
    * @param expressions the expressions in the block
    * @param returnValue the final expression in the block
    * @param location    the source location that the node corresponds to
    * @param suspended   whether or not the block is suspended
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Block(
    expressions: List[Expression],
    returnValue: Expression,
    location: Option[IdentifiedLocation],
    suspended: Boolean             = false,
    passData: MetadataStorage      = MetadataStorage(),
    diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Expression
      with IRKind.Primitive {
    var id: UUID @Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param expressions the expressions in the block
      * @param returnValue the final expression in the block
      * @param location    the source location that the node corresponds to
      * @param suspended   whether or not the block is suspended
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      expressions: List[Expression]        = expressions,
      returnValue: Expression              = returnValue,
      location: Option[IdentifiedLocation] = location,
      suspended: Boolean                   = suspended,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Block = {
      val res = Block(
        expressions,
        returnValue,
        location,
        suspended,
        passData,
        diagnostics
      )
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Block =
      copy(
        expressions = expressions.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        returnValue = returnValue.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Block =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Block = {
      copy(
        expressions = expressions.map(fn.asScala),
        returnValue = fn(returnValue)
      )
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Expression.Block(
         |expressions = $expressions,
         |returnValue = $returnValue,
         |location = $location,
         |suspended = $suspended,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = expressions :+ returnValue

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val newIndent = indent + indentLevel
      val expressionsStr = expressions
        .map(mkIndent(newIndent) + _.showCode(newIndent))
        .mkString("\n")
      val returnStr = mkIndent(newIndent) + returnValue.showCode(newIndent)

      s"\n$expressionsStr\n$returnStr"
    }
  }

  /** A binding expression of the form `name = expr`
    *
    * To create a binding that binds no available name, set the name of the
    * binding to an [[Name.Blank]] (e.g. _ = foo a b).
    *
    * @param name        the name being bound to
    * @param expression  the expression being bound to `name`
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Binding(
    name: Name,
    expression: Expression,
    location: Option[IdentifiedLocation],
    passData: MetadataStorage      = MetadataStorage(),
    diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Expression
      with IRKind.Primitive {

    var id: UUID @Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param name        the name being bound to
      * @param expression  the expression being bound to `name`
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name                           = name,
      expression: Expression               = expression,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Binding = {
      val res = Binding(name, expression, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Binding =
      copy(
        name = name.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        expression = expression.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Binding =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Binding = {
      copy(name = name.mapExpressions(fn), expression = fn(expression))
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Expression.Binding(
         |name = $name,
         |expression = $expression,
         |location = $location
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(name, expression)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"${name.showCode(indent)} = ${expression.showCode(indent)}"
  }
}
