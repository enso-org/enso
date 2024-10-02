package org.enso.compiler.pass;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.ProcessingPass;

/**
 * Mini IR pass operates on a single IR element at a time. The {@link org.enso.compiler.Compiler}
 * traverses the whole IR tree in DFS. It works in two phases.
 *
 * <p>In the first, <b>prepare</b> phase, the compiler traverses from the root to the leaves and
 * calls the {@link #prepare(IR)} method on the mini pass. During this phase, the mini pass can
 * gather information about the current IR element, but not modify it.
 *
 * <p>In the second, <b>transform</b> phase, the compiler returns from the leaves to the root and
 * calls the {@link #transformIr(IR)} method on the mini pass. During this phase, the mini pass is
 * free to transform the current IR element. The children of the current IR element are already
 * transformed.
 *
 * <p>For each IR element:
 *
 * <ol>
 *   <li>The {@link #prepare(IR)} method is called to prepare the pass for the current IR element.
 *       This method is called when the {@link org.enso.compiler.Compiler} traverses the IR tree
 *       from top to bottom. This is useful for mini passes that need to build some information
 *       about the current IR element before transforming it. The mini pass must not modify the IR
 *       element neither attach any metadata to it in this method. By returning {@code null} from
 *       this method, the mini pass signals to the compiler that it wishes to not process the
 *       subtree of the current IR element.
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
  /**
   * Prepare the pass for the provided IR element. This method is called when the {@link
   * org.enso.compiler.Compiler} traverses the IR element from top to bottom.
   *
   * <p>The mini pass is free to gather any information about the elements it encounters (via this
   * method) and use it in the {@link #transformIr(IR)} method. Note however, that it is not wise to
   * store the references to the IR or their children for later comparison in the {@link
   * #transformIr(IR) transform phase}, as the IR tree will most likely be transformed during the
   * compilation process.
   *
   * <p>TL;DR; Do no store references to the IR elements or their children in this method.
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
