package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.CallTarget;
import org.enso.interpreter.EnsoLanguage;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.InlineableRootNode;

/** Root node for use by all the builtin functions. */
@NodeInfo(shortName = "BuiltinRoot", description = "Root node for builtin functions.")
public abstract class BuiltinRootNode extends RootNode implements InlineableRootNode {
  protected BuiltinRootNode(EnsoLanguage language) {
    super(language);
  }

  /**
   * Executes this node's logic, returning a pair of return value and the new state.
   *
   * @param frame current execution frame
   * @return the result value of executing the logic.
   */
  @Override
  public abstract Object execute(VirtualFrame frame);

  /**
   * Gets the source-level name of this node.
   *
   * @return the source-level name of the node
   */
  @Override
  public abstract String getName();

  /**
   * Factory method creating a {@link DirectCallNode} to invoke this builtin.Defaults to standard
   * {@link DirectCallNode#create(com.oracle.truffle.api.CallTarget)} implementation. Subclasses may
   * override this with the help of {@link InlinedCallNode}.
   *
   * @return new node to use to call this builtin
   */
  public DirectCallNode createDirectCallNode() {
    var callNode = DirectCallNode.create(cloneUninitialized().getCallTarget());
    callNode.forceInlining();
    return callNode;
  }

  /**
   * Helper class allowing better implementation of {@link #createDirectCallNode}. Subclass, pass in
   * {@code extra} and {@code body} and override {@code call} method to do what has to be done.
   *
   * @param <E> extra data to keep in the node
   * @param <N> node to delegate to from {@link #call(java.lang.Object...)} method
   */
  protected abstract static class InlinedCallNode<E, N extends Node> extends DirectCallNode {
    protected final E extra;
    @Child protected N body;

    protected InlinedCallNode(E extra, N body) {
      super(null);
      this.extra = extra;
      this.body = body;
    }

    @Override
    public abstract Object call(Object... arguments);

    @Override
    public final boolean isInlinable() {
      return true;
    }

    @Override
    public final boolean isInliningForced() {
      return true;
    }

    @Override
    public final void forceInlining() {}

    @Override
    public final boolean isCallTargetCloningAllowed() {
      return false;
    }

    @Override
    public final boolean cloneCallTarget() {
      return false;
    }

    @Override
    public final CallTarget getClonedCallTarget() {
      return getRootNode().getCallTarget();
    }
  }
}
