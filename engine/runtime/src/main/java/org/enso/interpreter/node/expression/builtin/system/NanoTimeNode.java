package org.enso.interpreter.node.expression.builtin.system;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "System",
    name = "nano_time",
    description = "Gets the nanosecond resolution system time.")
public final class NanoTimeNode extends Node {
  @CompilerDirectives.TruffleBoundary
  long execute(Object _this) {
    return System.nanoTime();
  }
}
