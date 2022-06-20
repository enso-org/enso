package org.enso.interpreter.runtime.tag;

import org.enso.compiler.core.IR;

/** Implemented by nodes that can be patched by small edits. */
public interface Patchable {
  /**
   * Computes new value this node shall return.
   *
   * @param ir the expression the node is being patched to
   * @return {@code null} if the patching cannot be done or an instance of {@link Runnable} signal
   *     patching is possible with {@link Runnable#run() } method to invoke to finish the patching
   */
  public Runnable parsePatch(IR.Expression ir);

  /** Tag to apply to nodes that support patching and implement {@link Patchable}. */
  public final class Tag extends com.oracle.truffle.api.instrumentation.Tag {}
}
