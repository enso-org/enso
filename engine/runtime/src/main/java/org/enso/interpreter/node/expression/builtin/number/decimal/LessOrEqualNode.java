package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "<=", description = "Comparison of numbers.")
public abstract class LessOrEqualNode extends Node {

  abstract Object execute(double self, Object that);

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
  DataflowError doOther(double self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var typeError = builtins.error().makeTypeError(builtins.number().getNumber(), that, "that");
    return DataflowError.withoutTrace(typeError, this);
  }
}
