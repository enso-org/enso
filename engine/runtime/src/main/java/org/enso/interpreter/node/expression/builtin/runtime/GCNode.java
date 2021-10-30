package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;

@BuiltinMethod(type = "Runtime", name = "gc", description = "Forces garbage collection")
public abstract class GCNode extends Node {

  public abstract Object execute(Object _this);

  /** @return A new GCNode. */
  public static GCNode build() {
    return GCNodeGen.create();
  }

  @Specialization
  Object doGc(Object _this, @CachedContext(Language.class) Context context) {
    runGC();
    return context.getBuiltins().nothing().newInstance();
  }

  @CompilerDirectives.TruffleBoundary
  private void runGC() {
    System.gc();
  }
}
