package org.enso.compiler.core.ir
package expression

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.IR.{indentLevel, mkIndent}

import java.util.UUID

/** The Enso case expression. */
case class IfThenElse(
  cond: Expression,
  trueBranch: Expression,
  private val falseBranchOrNull: Expression,
  override val identifiedLocation: IdentifiedLocation,
  override val passData: MetadataStorage = new MetadataStorage()
) extends Expression
    with IRKind.Primitive
    with LazyDiagnosticStorage
    with LazyId {

  cond.getClass()
  trueBranch.getClass()

  def falseBranch(): Option[Expression] = Option(falseBranchOrNull)

  /** Creates a copy of `this`.
    *
    * @param cond   the expression whose value is being matched on
    * @param branches    the branches of the case expression
    * @param isNested    if true, the flag indicates that the expr represents a desugared nested case
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    * @param id          the identifier for the new node
    * @return a copy of `this`, updated with the specified values
    */
  def copy(
    cond: Expression                     = cond,
    trueBranch: Expression               = trueBranch,
    falseBranchOrNull: Expression        = falseBranchOrNull,
    location: Option[IdentifiedLocation] = location,
    passData: MetadataStorage            = passData,
    diagnostics: DiagnosticStorage       = diagnostics,
    id: UUID @Identifier                 = id
  ): IfThenElse = {
    if (
      cond != this.cond
      || trueBranch != this.trueBranch
      || falseBranchOrNull != this.falseBranchOrNull
      || location != this.location
      || passData != this.passData
      || diagnostics != this.diagnostics
      || id != this.id
    ) {
      val res = new IfThenElse(
        cond,
        trueBranch,
        falseBranchOrNull,
        location.orNull,
        passData
      )
      res.diagnostics = diagnostics
      res.id          = id
      res
    } else this
  }

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): IfThenElse = {
    copy(
      cond       = fn(cond),
      trueBranch = fn(trueBranch),
      falseBranchOrNull =
        if (falseBranchOrNull ne null) fn(falseBranchOrNull) else null
    )
  }

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): IfThenElse =
    copy(location = location)

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): IfThenElse = {
    copy(
      cond = cond.duplicate(
        keepLocations,
        keepMetadata,
        keepDiagnostics,
        keepIdentifiers
      ),
      trueBranch = trueBranch.duplicate(
        keepLocations,
        keepMetadata,
        keepDiagnostics,
        keepIdentifiers
      ),
      falseBranchOrNull =
        if (falseBranchOrNull ne null)
          falseBranchOrNull.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        else null,
      location = if (keepLocations) location else None,
      passData =
        if (keepMetadata) passData.duplicate else new MetadataStorage(),
      diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
      id          = if (keepIdentifiers) id else null
    )
  }

  /** String representation. */
  override def toString: String =
    s"""
         |IfThenElse(
         |cond = $cond,
         |trueBranch = $trueBranch,
         |falseBranch = ${falseBranch()},
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

  /** @inheritdoc */
  override def children: List[IR] =
    List(cond, trueBranch) ++ falseBranch().toList

  /** @inheritdoc */
  override def showCode(indent: Int): String = {
    val newIndent = indent + indentLevel
    val headerStr =
      s"if ${cond.showCode(indent)} then\n${trueBranch.showCode(newIndent)}"
    val elseStr =
      if (falseBranchOrNull ne null)
        s"${mkIndent(indent)}else\n${falseBranchOrNull.showCode(newIndent)}"
      else ""
    s"$headerStr\n$elseStr"
  }

}
