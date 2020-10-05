package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

import java.lang.ref.WeakReference;

@BuiltinMethod(type = "Runtime", name = "gc", description = "Forces garbage collection")
public class GCNode extends Node {
  @CompilerDirectives.TruffleBoundary
  Object execute(Object _this) {
    Object obj = new Object();
    WeakReference<Object> weak = new WeakReference<>(obj);
    obj = null;
    long it = 0;
    while (weak.get() != null) {
      System.gc();
      it++;
    }
    try {
      Thread.sleep(100);
    } catch (InterruptedException e) {

    }
    System.out.println("Forcing GC took " + it + " calls");
    return 0;
  }
}
