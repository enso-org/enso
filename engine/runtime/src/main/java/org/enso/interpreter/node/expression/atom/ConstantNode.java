package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

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
  public Object execute(VirtualFrame frame) {
    return constant;
  }
}
