package org.enso.compiler.pass;

import org.enso.compiler.core.ir.ProcessingPass;
import scala.collection.immutable.Seq;

/**
 * A generic {@link IR} processing pass. Currently with two subclasses: classical {@link IRPass mega
 * IR processing pass} and {@link MiniPassFactory}.
 */
public interface IRProcessingPass extends ProcessingPass {
  /** The passes that this pass depends _directly_ on to run. */
  public Seq<? extends IRProcessingPass> precursorPasses();

  /**
   * The passes that are invalidated by running this pass.
   *
   * <p>If {@code P1} invalidates {@code P2}, and {@code P1} is a precursor of {@code P2}, then
   * {@code P1} must finish before {@code P2} starts.
   */
  public Seq<? extends IRProcessingPass> invalidatedPasses();
}
