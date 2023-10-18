package org.enso.compiler.core.ir
package expression

import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.IR.{randomId, Identifier, ToStringHelper}
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.{ir, IR}

/** A trait for all errors in Enso's IR. */
trait Error extends Expression with ir.module.scope.Definition with Diagnostic {

  /** @inheritdoc */
  override def mapExpressions(fn: Expression => Expression): Error

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Error

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
    * @param ir          the IR that is invalid
    * @param passData    any annotations from compiler passes
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class InvalidIR(
    ir: IR,
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Error
      with Diagnostic.Kind.Static
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

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
      id: Identifier                 = id
    ): InvalidIR = {
      val res = InvalidIR(ir, passData, diagnostics)
      res.id = id
      res
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
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): InvalidIR = this

    /** @inheritdoc */
    override val location: Option[IdentifiedLocation] = ir.location

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): InvalidIR =
      this

    /** @inheritdoc */
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
    override def message(source: Source): String =
      "InvalidIR: Please report this as a compiler bug."

    override def diagnosticKeys(): Array[Any] = Array()

    /** @inheritdoc */
    override def showCode(indent: Int): String = "Invalid_Ir"
  }

}
