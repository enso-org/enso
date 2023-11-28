package org.enso.compiler.core.ir
package expression

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.IR.randomId

import java.util.UUID

/** Operator sections. */
sealed trait Section extends Operator {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Section

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Section

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Section
}

object Section {

  /** Represents a left operator section of the form `(arg op)`.
    *
    * @param arg         the argument (on the left of the operator)
    * @param operator    the operator
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Left(
    arg: CallArgument,
    operator: Name,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Section
      with IRKind.Sugar {
    var id: UUID @Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param arg         the argument (on the left of the operator)
      * @param operator    the operator
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      arg: CallArgument                    = arg,
      operator: Name                       = operator,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Left = {
      val res = Left(arg, operator, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Left =
      copy(
        arg = arg.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        operator = operator
          .duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Left =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Section =
      copy(
        arg      = arg.mapExpressions(fn),
        operator = operator.mapExpressions(fn)
      )

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Section.Left(
         |arg = $arg,
         |operator =  $operator,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(arg, operator)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(${arg.showCode(indent)} ${operator.showCode(indent)})"
  }

  /** Represents a sides operator section of the form `(op)`
    *
    * @param operator    the operator
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Sides(
    operator: Name,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Section
      with IRKind.Sugar {
    var id: UUID @Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param operator    the operator
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      operator: Name                       = operator,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Sides = {
      val res = Sides(operator, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Sides =
      copy(
        operator = operator
          .duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Sides = copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Section =
      copy(operator = operator.mapExpressions(fn))

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Section.Sides(
         |operator =  $operator,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(operator)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(${operator.showCode(indent)})"
  }

  /** Represents a right operator section of the form `(op arg)`
    *
    * @param operator    the operator
    * @param arg         the argument (on the right of the operator)
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Right(
    operator: Name,
    arg: CallArgument,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Section
      with IRKind.Sugar {
    var id: UUID @Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param operator    the operator
      * @param arg         the argument (on the right of the operator)
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      operator: Name                       = operator,
      arg: CallArgument                    = arg,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Right = {
      val res = Right(operator, arg, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Right =
      copy(
        operator = operator
          .duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
        arg = arg.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Right = copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Section = {
      copy(
        operator = operator.mapExpressions(fn),
        arg      = arg.mapExpressions(fn)
      )
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Section.Right(
         |operator =  $operator,
         |arg = $arg,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(operator, arg)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(${operator.showCode(indent)} ${arg.showCode(indent)})"
  }
}
