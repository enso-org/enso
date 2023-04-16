package org.enso.interpreter.node;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Special interface that allows various {@link RootNode} subclasses to provide
 * more effective implementation of {@link DirectCallNode}. Used by for example
 * by {@code BuiltinRootNode}.
 */
public interface InlineableRootNode {
  /**
   * Provides access to {@link RootNode}. Usually the object shall inherit from
   * {link RootNode} as well as implement the {@link InlineableRootNode}
   * interface. This method thus usually returns {@code this}.
   *
   * @return {@code this} types as {link RootNode}
   */
  public RootNode getRootNode();

  /**
   * Name of the {@link RootNode}.
   *
   * @return root node name
   */
  public String getName();

  /**
   * Override to provide more effective implementation of {@link DirectCallNode}
   * suited more for Enso aggressive inlining.
   *
   * @return a node to {@link DirectCallNode#call(java.lang.Object...) call} the
   * associated {@link RootNode} - may return {@code null}
   */
  public DirectCallNode createDirectCallNode();

  /**
   * * Obtain a {@link DirectCallNode} for given {@link CallTarget}.Either
   * delegates to {@link #createDirectCallNode} or uses regular
   * {@link DirectCallNode#create(com.oracle.truffle.api.CallTarget)} method.
   * Use for example by {@code ExecuteCallNode}.
   *
   * @param target call target with regular or
   * {@link InlineableRootNode} {@link RootCallTarget#getRootNode()}
   * @return instance of {@link DirectCallNode} to use to invoke the
   * {@link RootNode#execute(com.oracle.truffle.api.frame.VirtualFrame)}.
   */
  public static DirectCallNode create(RootCallTarget target) {
    if (target.getRootNode() instanceof InlineableRootNode inRoot && inRoot.createDirectCallNode() instanceof DirectCallNode node) {
      return node;
    }
    return DirectCallNode.create(target);
  }
}
