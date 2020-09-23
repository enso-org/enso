package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.data.text.ConcatRope;
import org.enso.interpreter.runtime.data.text.Text;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.concurrent.locks.Lock;

public class ToJavaStringNode extends Node {
  private static ToJavaStringNode UNCACHED = new ToJavaStringNode();

  public static ToJavaStringNode getUncached() {
    return UNCACHED;
  }

  public static ToJavaStringNode build() {
    return new ToJavaStringNode();
  }

  public String execute(Text text) {
    if (text.isFlat()) {
      return (String) text.getContents();
    } else {
      return inplaceFlatten(text);
    }
  }

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
