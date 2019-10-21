package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.Context;

import java.io.OutputStream;
import java.io.PrintStream;

/** This node allows for printing the result of an arbitrary expression to standard output. */
@NodeInfo(shortName = "print", description = "Prints the value of child expression.")
public abstract class PrintNode extends ExpressionNode {
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
   * @return unit {@link Builtins#UNIT unit} type
   */
  @Specialization
  public Object doPrint(VirtualFrame frame, @CachedContext(Language.class) Context ctx) {
    doPrint(ctx.getOut(), expression.executeGeneric(frame));

    return Builtins.UNIT.newInstance();
  }

  /**
   * Prints the provided value to standard output.
   *
   * @param object the value to print
   */
  @CompilerDirectives.TruffleBoundary
  private void doPrint(PrintStream out, Object object) {
    out.println(object);
  }
}
