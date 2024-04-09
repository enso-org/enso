package org.enso.compiler.core.ir
package expression

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}

import java.util.UUID

/** Enso comment entities. */
sealed trait Comment extends Expression with module.scope.Definition {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Comment

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Comment

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Comment
}

object Comment {

  /** A documentation comment in the Enso source.
    *
    * @param doc         the documentation entity
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Documentation(
    doc: String,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Comment
      with IRKind.Primitive
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param doc         the documentation of `commented`
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      doc: String                          = doc,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Documentation = {
      val res = Documentation(doc, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Documentation =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Documentation = copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Documentation = this

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Comment.Documentation(
         |doc = $doc,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"## $doc"
  }
}
