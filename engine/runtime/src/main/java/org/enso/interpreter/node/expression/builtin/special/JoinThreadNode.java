package org.enso.interpreter.node.expression.builtin.special;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "", name = "<join_thread>")
public class JoinThreadNode extends Node {
  public Object execute(Object thread) {
    try {
      ((Thread) thread).join();
    } catch (InterruptedException e) {
    }
    return null;
  }
}
