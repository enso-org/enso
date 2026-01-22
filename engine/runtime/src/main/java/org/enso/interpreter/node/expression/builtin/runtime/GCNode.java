package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(
    type = "Runtime",
    name = "gc",
    description = "Forces garbage collection & other cleanups",
    autoRegister = false)
public final class GCNode extends Node {
  private GCNode() {}

  @CompilerDirectives.TruffleBoundary
  public Object execute(boolean flushCaches) {
    var ctx = EnsoContext.get(this);
    if (flushCaches) {
      ctx.getResourceManager().scheduleFinalizationOfSystemReferences();
    }
    System.gc();
    return ctx.getBuiltins().nothing();
  }

  /**
   * @return A new GCNode.
   */
  static GCNode build() {
    return new GCNode();
  }
}
