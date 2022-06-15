package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "<=", description = "Comparison of numbers.")
public abstract class LessOrEqualNode extends Node {

  abstract boolean execute(double self, Object that);

  static LessOrEqualNode build() {
    return LessOrEqualNodeGen.create();
  }

  @Specialization
  boolean doDouble(double self, double that) {
    return self <= that;
  }

  @Specialization
  boolean doLong(double self, long that) {
    return self <= (double) that;
  }

  @Specialization
  boolean doBigInteger(double self, EnsoBigInteger that) {
    return self <= BigIntegerOps.toDouble(that.getValue());
  }

  @Fallback
  boolean doOther(double self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    Atom number = builtins.number().getNumber().newInstance();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
