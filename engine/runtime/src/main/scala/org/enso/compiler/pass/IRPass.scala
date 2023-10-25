package org.enso.compiler.pass

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.{CompilerError, IR, Identifier}
import org.enso.compiler.core.ir.ProcessingPass
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.Expression
import shapeless.=:!=

import java.util.UUID
import scala.annotation.unused
import scala.reflect.ClassTag

/** A representation of a compiler pass that runs on the [[IR]] type.
  *
  * Passes that depend on the metadata of other passes should pull this metadata
  * directly from the IR, and not depend on metadata available in the context.
  *
  * Every pass should be implemented as a `case object` and should document in
  * its header the requirements it has for pass configuration and for passes
  * that must run before it.
  */
trait IRPass extends ProcessingPass {

  /** An identifier for the pass. Useful for keying it in maps. */
  val key: UUID @Identifier = IRPass.genId

  /** The type of the metadata object that the pass writes to the IR. */
  type Metadata <: ProcessingPass.Metadata

  /** The type of configuration for the pass. */
  type Config <: IRPass.Configuration

  /** The passes that this pass depends _directly_ on to run. */
  val precursorPasses: Seq[IRPass]

  /** The passes that are invalidated by running this pass. */
  val invalidatedPasses: Seq[IRPass]

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  def runModule(ir: Module, moduleContext: ModuleContext): Module

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression

  /** Updates the metadata in a copy of the IR when updating that metadata
    * requires global state.
    *
    * This is usually the case in the presence of structures that are shared
    * throughout the IR, and need to maintain that sharing for correctness. This
    * must be called with `copyOfIr` as the result of an `ir.duplicate` call.
    *
    * Additionally this method _must not_ alter the structure of the IR. It
    * should only update its metadata.
    *
    * @param sourceIr the IR being copied
    * @param copyOfIr a duplicate of `sourceIr`
    * @tparam T the concrete [[IR]] type
    * @return the result of updating metadata in `copyOfIr` globally using
    *         information from `sourceIr`
    */
  def updateMetadataInDuplicate[T <: IR](@unused sourceIr: T, copyOfIr: T): T =
    copyOfIr
}
object IRPass {

  /** Generates a pass identifier.
    *
    * @return a new pass identifier
    */
  def genId: UUID @Identifier = {
    UUID.randomUUID()
  }

  /** A representation of configuration for a given pass.
    *
    * Configuration must be able to be compared for equality.
    */
  trait Configuration {

    /** Whether or not the pass should write to the context. */
    var shouldWriteToContext: Boolean
  }
  object Configuration {
    case class Default() extends Configuration {
      override var shouldWriteToContext: Boolean = false
    }
  }

  /** This trait should be implemented by all metadata elements generated by
    * passes such that it can be stored in each IR node.
    *
    * All metadata instances must be [[Serializable]], but are guaranteed to
    * have `prepareForSpecialization` called before being serialised. Similarly,
    * they are guaranteed to have `restoreFromSerialization` called after they
    * have been deserialized and before any other operations occur.
    */
  trait IRMetadata extends ProcessingPass.Metadata {

    type Metadata = IRMetadata
    type Compiler = org.enso.compiler.Compiler

    /** The name of the metadata as a string. */
    val metadataName: String

    /** Casts the pass to the provided type.
      *
      * @param ev ensures that the pass type must be specified
      * @tparam T the type to cast to
      * @return `ev`, cast to `T` if it is a `T`
      */
    def as[T <: Metadata: ClassTag](implicit
      @unused ev: T =:!= Metadata
    ): Option[T] = {
      this match {
        case p: T => Some(p)
        case _    => None
      }
    }

    /** Unsafely casts the pass to the provided type.
      *
      * @param ev ensures that the pass type must be specified
      * @tparam T the type to cast to
      * @throws CompilerError if `this` is not a `T`
      * @return `this` as a `T`
      */
    @throws[CompilerError]
    def unsafeAs[T <: Metadata: ClassTag](implicit
      @unused ev: T =:!= Metadata
    ): T = {
      this
        .as[T]
        .getOrElse(
          throw new CompilerError(s"Cannot cast $this to the requested type.")
        )
    }

    /** Creates a duplicate of this metadata if applicable.
      *
      * This method should employ deep-copy semantics where appropriate. It may
      * return None to indicate that this metadata should not be preserved
      * during duplication.
      *
      * @return Some duplicate of this metadata or None if this metadata should
      *         not be preserved
      */
    def duplicate(): Option[Metadata]
  }
  object Metadata {

    /** An empty metadata type for passes that do not create any metadata. */
    sealed case class Empty() extends IRMetadata {
      override val metadataName: String = "Empty"

      /** @inheritdoc */
      override def prepareForSerialization(compiler: Compiler): Empty = this

      /** @inheritdoc */
      override def restoreFromSerialization(compiler: Compiler): Option[Empty] =
        Some(this)

      /** @inheritdoc */
      override def duplicate(): Option[Metadata] = Some(this)
    }
  }
}
