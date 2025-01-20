package org.enso.compiler.core.ir
package expression

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.{ir, IR, Identifier}

import java.util.UUID

/** A trait for all errors in IR. */
trait Error extends Expression with ir.module.scope.Definition with Diagnostic {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Error

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Error

  /** @inheritdoc */
  override def location: Option[IdentifiedLocation] =
    super[Expression].location()

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Error
}

object Error {

  /** A representation of an invalid piece of IR.
    *
    * @param ir the IR that is invalid
    * @param passData any annotations from compiler passes
    */
  sealed case class InvalidIR(
    ir: IR,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Error
      with Diagnostic.Kind.Static
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param ir          the IR that is invalid
      * @param passData    any annotations from compiler passes
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      ir: IR                         = ir,
      passData: MetadataStorage      = passData,
      diagnostics: DiagnosticStorage = diagnostics,
      id: UUID @Identifier           = id
    ): InvalidIR = {
      if (
        ir != this.ir
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = InvalidIR(ir, passData)
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
    ): InvalidIR =
      copy(
        ir = ir.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): InvalidIR = this

    /** @inheritdoc */
    override def identifiedLocation: IdentifiedLocation =
      ir.identifiedLocation()

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): InvalidIR =
      this

    /** String representation. */
    override def toString: String =
      s"""
         |Error.InvalidIR(
         |ir = $ir,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(ir)

    /** @inheritdoc */
    override def message(source: (IdentifiedLocation => String)): String =
      "InvalidIR: Please report this as a compiler bug."

    override def diagnosticKeys(): Array[Any] = Array()

    /** @inheritdoc */
    override def showCode(indent: Int): String = "Invalid_Ir"
  }

}
