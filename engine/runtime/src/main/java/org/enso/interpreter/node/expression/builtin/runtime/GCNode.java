package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;

@BuiltinMethod(
    type = "Runtime",
    name = "gc",
    description = "Forces garbage collection",
    autoRegister = false)
public abstract class GCNode extends Node {

  public abstract Object execute();

  /** @return A new GCNode. */
  public static GCNode build() {
    return GCNodeGen.create();
  }

  @Specialization
  Object doGc() {
    runGC();
    return Context.get(this).getBuiltins().nothing();
  }

  @CompilerDirectives.TruffleBoundary
  private void runGC() {
    System.gc();
  }
}
