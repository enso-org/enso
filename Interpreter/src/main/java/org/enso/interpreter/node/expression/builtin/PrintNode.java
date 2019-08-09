package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** This node allows for printing the result of an arbitrary expression to standard output. */
@NodeInfo(shortName = "print", description = "Prints the value of child expression.")
public final class PrintNode extends ExpressionNode {
  @Child private ExpressionNode expression;

  /**
   * Creates a node that prints the result of its expression.
   *
   * @param expression the expression to print the result of
   */
  public PrintNode(ExpressionNode expression) {
    this.expression = expression;
  }

  /**
   * Executes the print node.
   *
   * @param frame the stack frame for execution
   * @return unit {@link AtomConstructor#UNIT unit} type
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    doPrint(expression.executeGeneric(frame));

    return AtomConstructor.UNIT.newInstance();
  }

  /**
   * Prints the provided value to standard output.
   *
   * @param object the value to print
   */
  @CompilerDirectives.TruffleBoundary
  private void doPrint(Object object) {
    System.out.println(object);
  }
}
