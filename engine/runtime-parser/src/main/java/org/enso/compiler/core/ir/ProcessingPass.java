package org.enso.compiler.core.ir;

import org.enso.compiler.core.CompilerStub;
import scala.Option;

public interface ProcessingPass {
  public interface Metadata {
    /** The name of the metadata as a string. */
    String metadataName();

    /**
     * Prepares the metadata for serialization.
     *
     * <p>Metadata prepared for serialization should not contain any links that span more than one
     * module, or any other properties that are problematic when serialized.
     *
     * <p>Due to the type safety properties of [[org.enso.compiler.core.ir.MetadataStorage]], to
     * allow this conversion to work it must be type-refined to return `typeof this`. To that end,
     * there is no default definition for this method.
     *
     * @param compiler the Enso compiler
     * @return `this`, but prepared for serialization
     */
    <Compiler extends CompilerStub> Metadata prepareForSerialization(Compiler compiler);

    /**
     * Restores metadata after it has been deserialized.
     *
     * <p>Due to the type safety properties of [[org.enso.compiler.core.ir.MetadataStorage]], to
     * allow this conversion to work it must be type-refined to return `typeof this`. To that end,
     * there is no default definition for this method.
     *
     * @param compiler the Enso compiler
     * @return `this`, but restored from serialization, or None if restoration could not be
     *     performed
     */
    <Compiler extends CompilerStub> Option<Metadata> restoreFromSerialization(Compiler compiler);

    /**
     * Creates a duplicate of this metadata if applicable.
     *
     * <p>This method should employ deep-copy semantics where appropriate. It may return None to
     * indicate that this metadata should not be preserved during duplication.
     *
     * @return Some duplicate of this metadata or None if this metadata should not be preserved
     */
    Option<Metadata> duplicate();
  }
}
