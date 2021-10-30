package org.enso.interpreter.node.expression.builtin.system;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "System",
    name = "exit",
    description = "Exits the process, returning the provided code.")
public class ExitNode extends Node {
  @CompilerDirectives.TruffleBoundary
  Object execute(Object _this, long code) {
    System.exit((int) code);
    return null;
  }
}
