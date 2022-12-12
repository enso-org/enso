package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Set;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;

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

  /**
   * Wrap all the statements inside this block node in {@link StatementNode}. Care is taken not for
   * wrapping expression twice.
   *
   * @return This BlockNode with all the statements wrapped.
   */
  @Override
  public InstrumentableNode materializeInstrumentableNodes(
      Set<Class<? extends Tag>> materializedTags) {
    if (materializedTags.contains(StandardTags.StatementTag.class)) {
      var ctx = EnsoContext.get(this);
      if (ctx != null) {
        Assumption chromeInspectorNotAttached = ctx.getChromeInspectorNotAttached();
        if (chromeInspectorNotAttached.isValid()
            && ctx.getEnvironment().getInstruments().containsKey("inspect")) {
          chromeInspectorNotAttached.invalidate("Chrome inspector attached");
        }
      }
      for (int i = 0; i < statements.length; i++) {
        if (!isNodeWrapped(statements[i])) {
          statements[i] = insert(StatementNode.wrap(statements[i]));
        }
      }
      if (!isNodeWrapped(returnExpr)) {
        returnExpr = insert(StatementNode.wrap(returnExpr));
      }
    }
    return this;
  }

  private static boolean isNodeWrapped(ExpressionNode node) {
    return node instanceof StatementNode || ExpressionNode.isWrapper(node);
  }

  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    return super.hasTag(tag)
        || tag == StandardTags.RootBodyTag.class
        || tag == StandardTags.RootTag.class;
  }
}
