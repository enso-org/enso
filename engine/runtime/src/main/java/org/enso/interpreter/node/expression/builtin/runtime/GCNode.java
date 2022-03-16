package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;

@BuiltinMethod(type = "Runtime", name = "gc", description = "Forces garbage collection")
public abstract class GCNode extends Node {

  public abstract Object execute(Object _this);

  /** @return A new GCNode. */
  public static GCNode build() {
    return GCNodeGen.create();
  }

  @Specialization
  Object doGc(Object _this) {
    runGC();
    return Context.get(this).getBuiltins().nothing().newInstance();
  }

  @CompilerDirectives.TruffleBoundary
  private void runGC() {
    System.gc();
  }
}
