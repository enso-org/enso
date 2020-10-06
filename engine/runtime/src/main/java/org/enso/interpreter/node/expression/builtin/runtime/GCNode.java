package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type="Runtime", name="gc", description = "Forces garbage collection")
public class GCNode extends Node {
  @CompilerDirectives.TruffleBoundary
  Object execute(Object _this) {
    System.gc();
    return 0;
  }
}
