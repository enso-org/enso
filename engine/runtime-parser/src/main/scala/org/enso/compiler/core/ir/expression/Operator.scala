package org.enso.compiler.core.ir
package expression

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}

import java.util.UUID;

/** Operator applications in Enso. */
trait Operator extends Application {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Operator

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Operator

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Operator
}

object Operator {

  /** A representation of a generic binary operator application in Enso.
    *
    * @param left        the left operand to `operator`
    * @param operator    the operator function being called
    * @param right       the right operand to `operator`
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Binary(
    left: CallArgument,
    operator: Name,
    right: CallArgument,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Operator
      with IRKind.Sugar
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param left        the left operand to `operator`
      * @param operator    the operator function being called
      * @param right       the right operand to `operator`
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      left: CallArgument                   = left,
      operator: Name                       = operator,
      right: CallArgument                  = right,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Binary = {
      val res =
        Binary(left, operator, right, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Binary =
      copy(
        left = left.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        operator = operator.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        right = right.duplicate(
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
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Binary =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Binary = {
      copy(left = left.mapExpressions(fn), right = right.mapExpressions(fn))
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Operator.Binary(
         |left = $left,
         |operator = $operator,
         |right = $right,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(left, operator, right)

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val opStr = operator.showCode(indent)

      s"((${left.showCode(indent)}) $opStr (${right.showCode(indent)}))"
    }
  }
}
