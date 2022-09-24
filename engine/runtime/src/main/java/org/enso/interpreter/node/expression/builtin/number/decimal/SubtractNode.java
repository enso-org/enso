package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "-", description = "Subtraction of numbers.")
public abstract class SubtractNode extends Node {
  abstract double execute(double self, Object that);

  static SubtractNode build() {
    return SubtractNodeGen.create();
  }

  @Specialization
  double doDouble(double self, double that) {
    return self - that;
  }

  @Specialization
  double doLong(double self, long that) {
    return self - that;
  }

  @Specialization
  double doBigInteger(double self, EnsoBigInteger that) {
    return self - BigIntegerOps.toDouble(that.getValue());
  }

  @Fallback
  double doOther(double self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
