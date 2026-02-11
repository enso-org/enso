package org.enso.compiler.core.ir;

import scala.Option;

public interface ProcessingPass {
  public interface Metadata {
    /** The name of the metadata as a string. */
    String metadataName();

    /**
     * Creates a duplicate of this metadata if applicable.
     *
     * <p>This method should employ deep-copy semantics where appropriate. It may return None to
     * indicate that this metadata should not be preserved during duplication.
     *
     * @return Some duplicate of this metadata or None if this metadata should not be preserved
     */
    Option<? extends Metadata> duplicate();
  }
}
