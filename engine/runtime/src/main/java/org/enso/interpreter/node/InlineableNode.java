package org.enso.interpreter.node;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;

/**
 * More effective {@link DirectCallNode} alternative. Supports more aggressive inlining needed by
 * {@link ExecuteCallNode}.
 */
public abstract class InlineableNode extends Node {
  /**
   * Invokes the computation represented by the node.
   *
   * @param frame current frame of the caller
   * @param arguments arguments for the functionality
   * @return result of the computation
   */
  public abstract Object call(VirtualFrame frame, Object[] arguments);

  /**
   * Special interface that allows various {@link RootNode} subclasses to provide more effective
   * implementation of {@link DirectCallNode} alternative. Used by for example by {@code
   * BuiltinRootNode}.
   */
  public interface Root {
    /**
     * Provides access to {@link RootNode}. Usually the object shall inherit from {link RootNode} as
     * well as implement the {@link InlineableNode} interface. This method thus usually returns
     * {@code this}.
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
     * Override to provide more effective implementation of {@link DirectCallNode} alternative.
     * Suited more for Enso aggressive inlining.
     *
     * @return a node to call the associated {@link RootNode} - may return {@code null}
     */
    public InlineableNode createInlineableNode();
  }
}
