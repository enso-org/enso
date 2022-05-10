package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Set;
import org.enso.interpreter.node.ExpressionNode;

/**
 * This node defines the body of a function for execution, as well as the protocol for executing the
 * function body.
 */
@NodeInfo(shortName = "Block")
public class BlockNode extends ExpressionNode {
  @Children private final ExpressionNode[] statements;
  @Child private ExpressionNode returnExpr;

  private BlockNode(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    this.statements = expressions;
    this.returnExpr = returnExpr;
  }

  /**
   * Creates an instance of this node.
   *
   * @param expressions the function body
   * @param returnExpr the return expression from the function
   * @return a node representing a block expression
   */
  public static BlockNode build(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    return new BlockNode(expressions, returnExpr);
  }

  public static BlockNode buildSilent(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    return new BlockNode(expressions, returnExpr);
  }

  /**
   * Executes the body of the function.
   *
   * @param frame the stack frame for execution
   * @return the result of executing this function
   */
  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    for (ExpressionNode statement : statements) {
      statement.executeVoid(frame);
    }
    return returnExpr.executeGeneric(frame);
  }

  @Override
  public InstrumentableNode materializeInstrumentableNodes(
      Set<Class<? extends Tag>> materializedTags) {
    if (materializedTags.contains(StandardTags.StatementTag.class)) {
      for (int i = 0; i < statements.length; i++) {
        statements[i] = insert(StatementNode.wrap(statements[i]));
      }
      this.returnExpr = insert(StatementNode.wrap(returnExpr));
    }
    return this;
  }
}
