package org.enso.compiler.pass;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;

/**
 * Mini IR pass operates on a single IR element at a time. The {@link org.enso.compiler.Compiler}
 * traverses the whole IR tree in DFS. The actual work is done by {@link MiniIRPass} implementation.
 * This factory only contains a collection of factory methods to create such {@link MiniIRPass IR
 * mini passes}. If a mini pass supports only inline compilation, its {@link
 * #createForModuleCompilation(ModuleContext)} method should return null.
 */
public interface MiniPassFactory extends IRProcessingPass {
  /**
   * Creates an instance of mini pass that is capable of transforming IR elements in the context of
   * a module.
   *
   * @param moduleContext A mini pass can optionally save reference to this module context.
   * @return May return {@code null} if module compilation is not supported.
   */
  MiniIRPass createForModuleCompilation(ModuleContext moduleContext);

  /**
   * Creates an instance of mini pass that is capable of transforming IR elements in the context of
   * an inline compilation.
   *
   * @param inlineContext A mini pass can optionally save reference to this inline context.
   * @return Must not return {@code null}. Inline compilation should always be supported.
   */
  MiniIRPass createForInlineCompilation(InlineContext inlineContext);
}
