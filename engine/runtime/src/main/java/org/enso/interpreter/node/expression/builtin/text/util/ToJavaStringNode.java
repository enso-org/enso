package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.runtime.data.text.ConcatRope;
import org.enso.interpreter.runtime.data.text.Text;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.concurrent.locks.Lock;

@NodeInfo(description = "Converts Enso Text to a Java String.")
public class ToJavaStringNode extends Node {
  private static ToJavaStringNode UNCACHED = new ToJavaStringNode();

  /**
   * Returns the uncached version of this node.
   *
   * @return the uncached version of this node.
   */
  public static ToJavaStringNode getUncached() {
    return UNCACHED;
  }

  /**
   * Creates a new instance of this node.
   *
   * @return a new instance of this node.
   */
  public static ToJavaStringNode build() {
    return new ToJavaStringNode();
  }

  /**
   * Performs the conversion of Enso Text to a Java String.
   *
   * @param text the text to convert.
   * @return the result of conversion.
   */
  public String execute(Text text) {
    if (text.isFlat()) {
      return (String) text.getContents();
    } else {
      return inplaceFlatten(text);
    }
  }

  /**
   * Converts text to a Java String. For use outside of Truffle Nodes.
   *
   * @param text the text to convert.
   * @return the result of conversion.
   */
  @CompilerDirectives.TruffleBoundary
  public static String inplaceFlatten(Text text) {
    Lock lock = text.getLock();
    lock.lock();
    String result;
    try {
      if (text.isFlat()) {
        result = (String) text.getContents();
      } else {
        result = doFlatten(text);
      }
    } finally {
      lock.unlock();
    }
    return result;
  }

  private static String doFlatten(Text text) {
    Deque<Object> workStack = new ArrayDeque<>();
    StringBuilder bldr = new StringBuilder();
    workStack.push(text.getContents());
    while (!workStack.isEmpty()) {
      Object item = workStack.pop();
      if (item instanceof String) {
        bldr.append((String) item);
      } else {
        ConcatRope rope = (ConcatRope) item;
        workStack.push(rope.getRight());
        workStack.push(rope.getLeft());
      }
    }
    String res = bldr.toString();
    text.setContents(res);
    text.setFlat(true);
    return res;
  }
}
