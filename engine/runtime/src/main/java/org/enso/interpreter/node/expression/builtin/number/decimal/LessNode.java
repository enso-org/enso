package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "<", description = "Comparison of numbers.")
public abstract class LessNode extends Node {

  abstract Object execute(double self, Object that);

  static LessNode build() {
    return LessNodeGen.create();
  }

  @Specialization
  Object doDouble(double self, double that) {
    return self < that;
  }

  @Specialization
  Object doLong(double self, long that) {
    return self < (double) that;
  }

  @Specialization
  Object doBigInteger(double self, EnsoBigInteger that) {
    return self < BigIntegerOps.toDouble(that.getValue());
  }

  @Fallback
  Object doOther(double self, Object that) {
    var builtins = Context.get(this).getBuiltins();
    var typeError = builtins.error().makeTypeError(builtins.number().getNumber(), that, "that");
    return that == builtins.nothing() ? DataflowError.withoutTrace(typeError, this) : DataflowError.withTrace(typeError, this);
  }
}
