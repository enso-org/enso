package org.enso.test.utils;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.function.Function;
import org.enso.interpreter.EnsoLanguage;

/**
 * An artificial RootNode. Used for tests of nodes that need to be adopted. Just create this root
 * node inside a context, all the other nodes, and insert them via {@link #insertChildren(Node...)}.
 */
public final class TestRootNode extends RootNode {

  private final Function<VirtualFrame, Object> callback;

  public TestRootNode() {
    this(null);
  }

  public TestRootNode(Function<VirtualFrame, Object> callback) {
    super(EnsoLanguage.get(null));
    this.callback = callback;
  }

  public void insertChildren(Node... children) {
    for (Node child : children) {
      insert(child);
    }
  }

  /** In the tests, do not execute this root node, but execute directly the child nodes. */
  @Override
  public Object execute(VirtualFrame frame) {
    if (callback == null) {
      throw new AssertionError("should not reach here");
    } else {
      return callback.apply(frame);
    }
  }
}
