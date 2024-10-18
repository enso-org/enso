package org.enso.compiler.core.ir
package expression

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.ir.MetadataStorage

import java.util.UUID

// === Foreign ==============================================================

/** Foreign code entities. */
sealed trait Foreign extends Expression {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Foreign

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Foreign

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Foreign
}

object Foreign {

  /** A foreign code definition in Enso.
    *
    * @param lang the foreign language being written
    * @param code the code written in `lang`
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class Definition(
    lang: String,
    code: String,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Foreign
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param lang        the foreign language being written
      * @param code        the code written in `lang`
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      lang: String                         = lang,
      code: String                         = code,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Definition = {
      if (
        lang != this.lang
        || code != this.code
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Definition(lang, code, location.orNull, passData)
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
    ): Definition =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Definition = copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Definition =
      this

    /** String representation. */
    override def toString: String =
      s"""
         |Foreign.Definition(
         |lang = $lang,
         |code = $code,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = "FOREIGN DEF"
  }
}
