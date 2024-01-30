package org.enso.compiler.core.ir
package expression

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.IR.{indentLevel, mkIndent}

import java.util.UUID

/** The Enso case expression. */
sealed trait Case extends Expression {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Case

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Case

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Case
}

object Case {

  /** The main body of the Enso case expression.
    *
    * @param scrutinee   the expression whose value is being matched on
    * @param branches    the branches of the case expression
    * @param isNested    if true, the flag indicates that the expr represents a desugared nested case
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Expr(
    scrutinee: Expression,
    branches: Seq[Branch],
    isNested: Boolean,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Case
      with IRKind.Primitive
      with LazyId {

    def this(
      scrutinee: Expression,
      branches: Seq[Branch],
      location: Option[IdentifiedLocation],
      passData: MetadataStorage,
      diagnostics: DiagnosticStorage
    ) = {
      this(scrutinee, branches, false, location, passData, diagnostics)
    }

    /** Creates a copy of `this`.
      *
      * @param scrutinee   the expression whose value is being matched on
      * @param branches    the branches of the case expression
      * @param isNested    if true, the flag indicates that the expr represents a desugared nested case
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      scrutinee: Expression                = scrutinee,
      branches: Seq[Branch]                = branches,
      isNested: Boolean                    = isNested,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Expr = {
      val res =
        Expr(scrutinee, branches, isNested, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Expr =
      copy(
        scrutinee = scrutinee.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        branches = branches.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        isNested = isNested,
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Expr =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Expr = {
      copy(
        scrutinee = fn(scrutinee),
        branches.map(_.mapExpressions(fn))
      )
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Case.Expr(
         |scrutinee = $scrutinee,
         |branches = $branches,
         |isNested = $isNested,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = scrutinee :: branches.toList

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val newIndent = indent + indentLevel
      val headerStr = s"case ${scrutinee.showCode(indent)} of"
      val branchesStr = branches
        .map(mkIndent(newIndent) + _.showCode(newIndent))
        .mkString("\n")

      s"$headerStr\n$branchesStr"
    }
  }

  object Expr {
    def apply(
      scrutinee: Expression,
      branches: Seq[Branch],
      location: Option[IdentifiedLocation]
    ): Expr =
      apply(
        scrutinee,
        branches,
        location,
        new MetadataStorage(),
        new DiagnosticStorage()
      )

    def apply(
      scrutinee: Expression,
      branches: Seq[Branch],
      location: Option[IdentifiedLocation],
      passData: MetadataStorage,
      diagnostics: DiagnosticStorage
    ): Expr = new Expr(scrutinee, branches, location, passData, diagnostics)
  }

  /** A branch in a case statement.
    *
    * @param pattern        the pattern that attempts to match against the scrutinee
    * @param expression     the expression that is executed if the pattern matches
    * @param terminalBranch the flag indicating whether the branch represents the final pattern to be checked
    * @param location       the source location that the node corresponds to
    * @param passData       the pass metadata associated with this node
    * @param diagnostics    compiler diagnostics for this node
    */
  sealed case class Branch(
    pattern: Pattern,
    expression: Expression,
    terminalBranch: Boolean,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Case
      with IRKind.Primitive
      with LazyId {

    def this(
      pattern: Pattern,
      expression: Expression,
      location: Option[IdentifiedLocation],
      passData: MetadataStorage,
      diagnostics: DiagnosticStorage
    ) = {
      this(pattern, expression, true, location, passData, diagnostics)
    }

    /** Creates a copy of `this`.
      *
      * @param pattern     the pattern that attempts to match against the scrutinee
      * @param expression  the expression that is executed if the pattern matches
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      pattern: Pattern                     = pattern,
      expression: Expression               = expression,
      terminalBranch: Boolean              = terminalBranch,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Branch = {
      val res = Branch(
        pattern,
        expression,
        terminalBranch,
        location,
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
    ): Branch =
      copy(
        pattern = pattern.duplicate(
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
        terminalBranch = terminalBranch,
        location       = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Branch =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Branch = {
      copy(pattern = pattern.mapExpressions(fn), expression = fn(expression))
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Case.Branch(
         |pattern = $pattern,
         |expression = $expression,
         |terminalBranch = $terminalBranch,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(pattern, expression)

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val newIndent = indent + indentLevel
      val bodyStr = if (expression.isInstanceOf[Expression.Block]) {
        s"\n${mkIndent(newIndent)}${expression.showCode(newIndent)}"
      } else {
        s"${expression.showCode(indent)}"
      }
      s"${pattern.showCode(indent)} -> $bodyStr"
    }
  }

  object Branch {
    def apply(
      pattern: Pattern,
      expression: Expression,
      location: Option[IdentifiedLocation]
    ): Branch =
      apply(
        pattern,
        expression,
        location,
        new MetadataStorage(),
        new DiagnosticStorage()
      )

    def apply(
      pattern: Pattern,
      expression: Expression,
      location: Option[IdentifiedLocation],
      passData: MetadataStorage,
      diagnostics: DiagnosticStorage
    ): Branch =
      new Branch(pattern, expression, location, passData, diagnostics)
  }
}
