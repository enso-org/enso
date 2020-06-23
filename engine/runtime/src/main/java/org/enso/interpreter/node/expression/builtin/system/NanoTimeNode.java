package org.enso.interpreter.node.expression.builtin.system;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "System",
    name = "nano_time",
    description = "Gets the nanosecond resolution system time.")
public final class NanoTimeNode extends Node {
  public long execute(Object self) {
    return getNanoTime();
  }

  @CompilerDirectives.TruffleBoundary
  private long getNanoTime() {
    return System.nanoTime();
  }
}
