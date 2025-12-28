package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.source.SourceSection;
import java.util.Set;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;

/**
 * This node defines the body of a function for execution, as well as the protocol for executing the
 * function body.
 */
@NodeInfo(shortName = "Block")
public class BlockNode extends ExpressionNode {
  private final BranchProfile unexpectedReturnValue;
  @Children private final ExpressionNode[] statements;
  @Child private ExpressionNode returnExpr;

  private BlockNode(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    this.statements = expressions;
    if (expressions.length > 0) {
      this.unexpectedReturnValue = BranchProfile.create();
    } else {
      this.unexpectedReturnValue = BranchProfile.getUncached();
    }
    this.returnExpr = returnExpr;
  }

  /**
   * Creates a "root tagged" instance of block node.
   *
   * @param expressions the function body
   * @param returnExpr the return expression from the function
   * @return a node representing a block expression
   */
  public static BlockNode buildRoot(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    return new Root(expressions, returnExpr);
  }

  /**
   * Creates a "root body tagged" instance of block node.
   *
   * @param expressions the function body
   * @param returnExpr the return expression from the function
   * @return a node representing a block expression
   */
  public static BlockNode buildRootBody(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    return new RootBody(expressions, returnExpr);
  }

  /**
   * Creates a block node with statements. When instrumented, the statements get wrapped in a into a
   * {@link StatementNode} so one can step over them.
   *
   * @param expressions the function body
   * @param returnExpr the return expression from the function
   * @return a node representing a block expression
   */
  public static BlockNode buildStatements(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    return new Statements(expressions, returnExpr);
  }

  /**
   * Creates a non-instrumented instance of block node. Used by {@code case of} and {@code if then
   * else} control flow constructs where the block itself shall be invisible to tooling.
   *
   * @param expressions the function body
   * @param returnExpr the return expression from the function
   * @return a node representing a block expression
   */
  public static BlockNode buildInvisible(ExpressionNode[] expressions, ExpressionNode returnExpr) {
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
    var ctx = EnsoContext.get(this);
    var nothing = ctx.getBuiltins().nothing();
    for (ExpressionNode statement : statements) {
      var result = statement.executeGeneric(frame);
      if (result != nothing) {
        unexpectedReturnValue.enter();
        if (result instanceof DataflowError err) {
          return err;
        }
      }
    }
    return returnExpr.executeGeneric(frame);
  }

  @Override
  public SourceSection getSourceSection() {
    var ss = super.getSourceSection();
    return ss != null ? ss : getRootNode().getSourceSection();
  }

  private static final class Root extends BlockNode {
    Root(ExpressionNode[] expressions, ExpressionNode returnExpr) {
      super(expressions, returnExpr);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
      if (super.hasTag(tag)) {
        return true;
      }
      return tag == StandardTags.RootTag.class;
    }
  }

  private static final class RootBody extends BlockNode {
    RootBody(ExpressionNode[] expressions, ExpressionNode returnExpr) {
      super(expressions, returnExpr);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
      if (super.hasTag(tag)) {
        return true;
      }
      return tag == StandardTags.RootBodyTag.class;
    }
  }

  private static final class Statements extends BlockNode {
    Statements(ExpressionNode[] expressions, ExpressionNode returnExpr) {
      super(expressions, returnExpr);
    }

    /**
     * Wrap all the statements inside this block node in {@link StatementNode}. Care is taken not
     * for wrapping expression twice.
     *
     * @return This BlockNode with all the statements wrapped.
     */
    @Override
    public InstrumentableNode materializeInstrumentableNodes(
        Set<Class<? extends Tag>> materializedTags) {
      if (materializedTags.contains(StandardTags.StatementTag.class)) {
        for (int i = 0; i < super.statements.length; i++) {
          if (!isNodeWrapped(super.statements[i])) {
            super.statements[i] = insert(StatementNode.wrap(super.statements[i]));
          }
        }
        if (!isNodeWrapped(super.returnExpr)) {
          super.returnExpr = insert(StatementNode.wrap(super.returnExpr));
        }
      }
      return this;
    }

    private static boolean isNodeWrapped(ExpressionNode node) {
      return node instanceof StatementNode || ExpressionNode.isWrapper(node);
    }
  }
}
