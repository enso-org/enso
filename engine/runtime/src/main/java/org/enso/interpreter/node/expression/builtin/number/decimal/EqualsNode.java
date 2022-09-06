package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "==", description = "Equality on numbers.")
public abstract class EqualsNode extends Node {
  abstract boolean execute(Object self, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doDouble(double self, double that) {
    return self == that;
  }

  @Specialization
  boolean doLong(double self, long that) {
    return self == (double) that;
  }

  @Specialization
  boolean doBigInteger(double self, EnsoBigInteger that) {
    return self == BigIntegerOps.toDouble(that.getValue());
  }

  @Fallback
  boolean doOther(Object self, Object that) {
    return self == that;
  }
}
