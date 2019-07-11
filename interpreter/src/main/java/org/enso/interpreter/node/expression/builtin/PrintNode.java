package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.StatementNode;

@NodeInfo(shortName = "print", description = "Prints the value of child expression.")
public final class PrintNode extends StatementNode {

  @Child private ExpressionNode expression;

  public PrintNode(ExpressionNode expression) {
    this.expression = expression;
  }

  @Override
  public void execute(VirtualFrame frame) {
    System.out.println(expression.executeGeneric(frame));
  }
}
