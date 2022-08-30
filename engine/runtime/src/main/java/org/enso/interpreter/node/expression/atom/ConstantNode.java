package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

public class ConstantNode extends RootNode {
  private final Object constant;

  /**
   * Creates a new instance of this node.
   *
   * @param language the current language instance.
   * @param atomConstructor the constructor to return.
   */
  public ConstantNode(TruffleLanguage<?> language, Object constant) {
    super(language);
    this.constant = constant;
  }

  /**
   * Executes the node, returning the predefined constructor.
   *
   * @param frame current execution frame
   * @return the constant constructor
   */
  public Stateful execute(VirtualFrame frame) {
    Object state = Function.ArgumentsHelper.getState(frame.getArguments());
    return new Stateful(state, constant);
  }
}
