package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.state.Stateful;

/** Root node for use by all the builtin functions. */
@NodeInfo(description = "Root node for builtin functions.")
public abstract class BuiltinRootNode extends RootNode {
  protected BuiltinRootNode(Language language) {
    super(language);
  }

  /**
   * Executes this node's logic, returning a pair of return value and the new state.
   *
   * @param frame current execution frame
   * @return the {@link Stateful} instance containing new state and the result value of executing
   *     the logic.
   */
  @Override
  public abstract Stateful execute(VirtualFrame frame);
}
