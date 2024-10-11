package org.enso.compiler.core.ir

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}

import java.util.UUID

/** A node representing an empty IR construct that can be used in any place.
  *
  * @param identifiedLocation the source location that the node corresponds to
  * @param passData the pass metadata associated with this node
  */
sealed case class Empty(
  override val identifiedLocation: IdentifiedLocation,
  override val passData: MetadataStorage = new MetadataStorage()
) extends IR
    with Expression
    with IRKind.Primitive
    with LazyDiagnosticStorage
    with LazyId {

  /** Creates a copy of `this`
    *
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    * @param id          the identifier for the new node
    * @return a copy of `this` with the specified fields updated
    */
  def copy(
    location: Option[IdentifiedLocation] = location,
    passData: MetadataStorage            = passData,
    diagnostics: DiagnosticStorage       = diagnostics,
    id: UUID @Identifier                 = id
  ): Empty = {
    if (
      location != this.location
      || passData != this.passData
      || diagnostics != this.diagnostics
      || id != this.id
    ) {
      val res = Empty(location.orNull, passData)
      res.diagnostics = diagnostics
      res.id          = id
      res
    } else this
  }

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Empty =
    copy(
      location = if (keepLocations) location else None,
      passData =
        if (keepMetadata) passData.duplicate else new MetadataStorage(),
      diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
      id          = if (keepIdentifiers) id else null
    )

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Empty =
    copy(location = location)

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Empty = this

  /** String representation. */
  override def toString: String =
    s"""
       |Empty(
       |location = $location,
       |passData = ${this.showPassData},
       |diagnostics = $diagnostics,
       |id = $id
       |)
       |""".toSingleLine

  /** @inheritdoc */
  override def children: List[IR] = List()

  /** @inheritdoc */
  override def showCode(indent: Int): String = "IR.Empty"
}
