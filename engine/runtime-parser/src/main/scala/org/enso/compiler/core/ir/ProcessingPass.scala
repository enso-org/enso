package org.enso.compiler.core.ir

import org.enso.compiler.core.CompilerStub

trait ProcessingPass extends Serializable {

  /** The type of the metadata object that the pass writes to the IR. */
  type Metadata <: ProcessingPass.Metadata

}

object ProcessingPass {
  trait Metadata extends Serializable {

    type Compiler <: CompilerStub

    /** The name of the metadata as a string. */
    val metadataName: String

    /** Prepares the metadata for serialization.
      *
      * Metadata prepared for serialization should not contain any links that
      * span more than one module, or any other properties that are problematic
      * when serialized.
      *
      * Due to the type safety properties of
      * [[org.enso.compiler.core.ir.MetadataStorage]], to allow this conversion
      * to work it must be type-refined to return `typeof this`. To that end,
      * there is no default definition for this method.
      *
      * @param compiler the Enso compiler
      * @return `this`, but prepared for serialization
      */
    def prepareForSerialization(compiler: Compiler): Metadata

    /** Restores metadata after it has been deserialized.
      *
      * Due to the type safety properties of
      * [[org.enso.compiler.core.ir.MetadataStorage]], to allow this conversion
      * to work it must be type-refined to return `typeof this`. To that end,
      * there is no default definition for this method.
      *
      * @param compiler the Enso compiler
      * @return `this`, but restored from serialization, or [[None]] if
      *         restoration could not be performed
      */
    def restoreFromSerialization(compiler: Compiler): Option[Metadata]

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
}
