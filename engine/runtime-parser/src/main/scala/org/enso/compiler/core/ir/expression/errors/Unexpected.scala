package org.enso.compiler.core.ir
package expression
package errors

import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.IR.randomId

import java.util.UUID

/** A trait for errors about unexpected language constructs. */
sealed trait Unexpected extends Error {

  /** The unexpected construct. */
  val ir: IR

  /** The name of the unexpected entity. */
  val entity: String

  override val location: Option[IdentifiedLocation] = ir.location

  /** @inheritdoc */
  override def message(source: Source): String = s"Unexpected $entity."

  /** @inheritdoc */
  override def diagnosticKeys(): Array[Any] = Array(entity)

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Unexpected

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Unexpected

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Unexpected
}

object Unexpected {

  /** An error representing a type signature not associated with a
    * binding of some kind.
    *
    * @param ir          the erroneous signature
    * @param passData    any pass metadata associated with this node
    * @param diagnostics any compiler diagnostics for this node
    */
  sealed case class TypeSignature(
    override val ir: IR,
    passData: MetadataStorage      = new MetadataStorage(),
    diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Unexpected
      with IRKind.Primitive
      with org.enso.compiler.core.ir.module.scope.Definition {
    override val entity: String = "type signature"

    var id: UUID @Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param ir          the erroneous signature
      * @param passData    any pass metadata associated with this node
      * @param diagnostics any compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      ir: IR                         = ir,
      passData: MetadataStorage      = passData,
      diagnostics: DiagnosticStorage = diagnostics,
      id: UUID @Identifier           = id
    ): TypeSignature = {
      val res = TypeSignature(ir, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): TypeSignature = this

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): TypeSignature = this

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): TypeSignature =
      copy(
        ir = ir.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def children: List[IR] = List(ir)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(Unexpected.TypeSignature ${ir.showCode(indent)})"
  }
}
