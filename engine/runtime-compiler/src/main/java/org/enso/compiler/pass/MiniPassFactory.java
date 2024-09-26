package org.enso.compiler.pass;

import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;

/**
 * A collection of factory methods for {@link MiniIRPass IR mini passes}. If a mini pass supports
 * only inline compilation, its {@link #createForModuleCompilation(ModuleContext)} method should
 * return null.
 */
public interface MiniPassFactory<T extends MiniIRPass> {
  /**
   * Creates an instance of mini pass that is capable of transforming IR elements in the context of
   * a module.
   *
   * @param moduleContext A mini pass can optionally save reference to this module context.
   * @return May return null if module compilation is not supported.
   */
  T createForModuleCompilation(ModuleContext moduleContext);

  /**
   * Creates an instance of mini pass that is capable of transforming IR elements in the context of
   * an inline compilation.
   *
   * @param inlineContext A mini pass can optionally save reference to this inline context.
   * @return Must not return null. Inline compilation should always be supported.
   */
  T createForInlineCompilation(InlineContext inlineContext);
}
