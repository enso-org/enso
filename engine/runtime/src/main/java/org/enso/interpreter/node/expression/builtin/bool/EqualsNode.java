package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Boolean", name = "==", description = "Computes the equality of two booleans")
public abstract class EqualsNode extends Node {
  abstract boolean execute(Object self, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doBoolean(boolean self, boolean that) {
    return self == that;
  }

  @Fallback
  boolean doOther(Object self, Object that) {
    return self == that;
  }
}
