package org.enso.interpreter.node.expression.builtin.special;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Special", name = "<join_thread>")
public class JoinThreadNode extends Node {
  public Object execute(Object self) {
    try {
      ((Thread) self).join();
    } catch (InterruptedException e) {
    }
    return null;
  }
}
