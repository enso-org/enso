package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;

/** Simple constant node that always results in the same {@link UnresolvedSymbol}. */
public class DynamicSymbolNode extends ExpressionNode {
  private final UnresolvedSymbol unresolvedSymbol;

  /**
   * Creates the node.
   *
   * @param unresolvedSymbol the symbol to always be this node's value.
   */
  public DynamicSymbolNode(UnresolvedSymbol unresolvedSymbol) {
    this.unresolvedSymbol = unresolvedSymbol;
  }

  /**
   * Gets the dynamic symbol from the node.
   *
   * @param frame the stack frame for execution
   * @return the constant {@link UnresolvedSymbol}
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return unresolvedSymbol;
  }
}
