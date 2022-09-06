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

@BuiltinMethod(type = "Decimal", name = "^", description = "Exponentiation of numbers.")
public abstract class PowNode extends Node {
  abstract double execute(double self, Object that);

  static PowNode build() {
    return PowNodeGen.create();
  }

  @Specialization
  double doDouble(double self, double that) {
    return Math.pow(self, that);
  }

  @Specialization
  double doLong(double self, long that) {
    return Math.pow(self, that);
  }

  @Specialization
  double doBigInteger(double self, EnsoBigInteger that) {
    return Math.pow(self, BigIntegerOps.toDouble(that.getValue()));
  }

  @Fallback
  double doOther(double self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
