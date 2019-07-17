package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.AtomConstructor;

@NodeInfo(shortName = "print", description = "Prints the value of child expression.")
public final class PrintNode extends ExpressionNode {

  @Child private ExpressionNode expression;

  public PrintNode(ExpressionNode expression) {
    this.expression = expression;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    doPrint(expression.executeGeneric(frame));

    return AtomConstructor.UNIT.newInstance();
  }

  @CompilerDirectives.TruffleBoundary
  private void doPrint(Object object) {
    System.out.println(object);
  }
}
