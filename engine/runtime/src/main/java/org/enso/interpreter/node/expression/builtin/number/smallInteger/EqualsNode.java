package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@BuiltinMethod(type = "Small_Integer", name = "==", description = "Equality on numbers.")
public abstract class EqualsNode extends Node {

  abstract boolean execute(Object self, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doLong(long self, long that) {
    return self == that;
  }

  @Specialization
  boolean doDouble(long self, double that) {
    return (double) self == that;
  }

  @Fallback
  boolean doOther(Object self, Object that) {
    return self == that;
  }
}
