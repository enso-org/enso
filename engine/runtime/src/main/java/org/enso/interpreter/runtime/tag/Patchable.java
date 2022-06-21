package org.enso.interpreter.runtime.tag;

import com.oracle.truffle.api.nodes.Node;
import java.util.function.Predicate;
import org.enso.compiler.core.IR;

/** Implemented by nodes that can be patched by small edits. */
public interface Patchable {
  /**
   * Reconfigures the node to be ready for patching.The returned callback interface can be called
   * later to inject new {@code IR.Expression} into the node. The callback returns {@code false} if
   * the patching cannot be done or {@code true} if the patching succeeded
   *
   * @return callback interface to communicate with
   * @param <N> node that also implements {@link Predicate} for patching
   */
  public <N extends Node & Predicate<IR.Expression>> N asPatchableNode();

  /** Tag to apply to nodes that support patching and implement {@link Patchable}. */
  public final class Tag extends com.oracle.truffle.api.instrumentation.Tag {}
}
