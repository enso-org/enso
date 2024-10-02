package org.enso.compiler.pass;

import java.util.Set;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.ProcessingPass;

/**
 * Mini IR pass operates on a single IR element at a time. The {@link org.enso.compiler.Compiler}
 * traverses the whole IR tree bottom-up from leaves to the root. For each IR element:
 *
 * <ol>
 *   <li>The {@link #prepare(IR)} method is called to prepare the pass for the current IR
 *       element. This method is called when the {@link org.enso.compiler.Compiler} traverses the
 *       element from top to bottom. This is useful for mini passes that need to build some
 *       information about the current IR element before transforming it.
 *   <li>The {@link #transformIr(IR)} method is called to transform the current IR element. This
 *       method is called when the {@link org.enso.compiler.Compiler} traverses the element from
 *       bottom to top. All the children of the current IR element are already transformed when this
 *       method is called.
 * </ol>
 *
 * <p>Inspired by: <a href="https://dl.acm.org/doi/10.1145/3140587.3062346">Miniphases: compilation
 * using modular and efficient tree transformations</a>. PDF available at <a
 * href="https://infoscience.epfl.ch/server/api/core/bitstreams/8ab72c0a-8aa6-4dee-a704-3504938dc316/content">infoscience.epfl.ch</a>
 */
public abstract class MiniIRPass implements ProcessingPass {

  /** Set of mini passes that should run before this mini pass. */
  public Set<MiniIRPass> runsAfter() {
    return Set.of();
  }

  /**
   * Prepare the pass for the provided IR element. This method is called when the
   * {@link org.enso.compiler.Compiler} traverses the IR element from top to bottom.
   *
   * @param current IR element to be prepared for transformation.
   * @return May be null if the pass does not want to traverse anything anymore.
   */
  public MiniIRPass prepare(IR current) {
    return this;
  }

  /**
   * Transform the provided IR element. Children of the IR element are already transformed when this
   * method is called. This method is called when the {@link org.enso.compiler.Compiler} traverses
   * the IR element from bottom to top.
   *
   * <p>The pass should not do any traversal in this method.
   *
   * @param ir IR element to be transformed by this pass.
   * @return The transformed IR, or the same IR if no transformation is needed. Must not return
   *     null.
   */
  public abstract IR transformIr(IR ir);

  public boolean checkPostCondition(IR ir) {
    return true;
  }
}
