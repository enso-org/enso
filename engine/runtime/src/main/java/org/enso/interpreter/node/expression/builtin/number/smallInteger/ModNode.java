package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "%", description = "Modulo division of numbers.")
public abstract class ModNode extends Node {
  abstract Object execute(long self, Object that);

  static ModNode build() {
    return ModNodeGen.create();
  }

  @Specialization
  Object doLong(long self, long that) {
    try {
      return self % that;
    } catch (ArithmeticException e) {
      return DataflowError.withoutTrace(
          Context.get(this).getBuiltins().error().getDivideByZeroError(), this);
    }
  }

  @Specialization
  double doDouble(long self, double that) {
    // No need to trap, as floating-point modulo returns NaN for division by zero instead of
    // throwing.
    return self % that;
  }

  @Specialization
  long doBigInteger(long self, EnsoBigInteger that) {
    // No need to trap, as 0 is never represented as an EnsoBigInteger.
    return self;
  }

  @Fallback
  Object doOther(long self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    var number = builtins.number().getNumber();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
