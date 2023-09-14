package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "<=", description = "Comparison of numbers.")
public abstract class LessOrEqualNode extends FloatNode {

  abstract Object execute(double self, Object that);

  static LessOrEqualNode build() {
    return LessOrEqualNodeGen.create();
  }

  @Specialization
  Object doDouble(double self, double that) {
    if (Double.isNaN(self) || Double.isNaN(that)) {
      return incomparableError(self, that);
    } else {
      return self <= that;
    }
  }

  @Specialization
  Object doLong(double self, long that) {
    if (Double.isNaN(self)) {
      return incomparableError(self, that);
    } else {
      return self <= (double) that;
    }
  }

  @Specialization
  Object doBigInteger(double self, EnsoBigInteger that) {
    if (Double.isNaN(self)) {
      return incomparableError(self, that);
    } else {
      return self <= BigIntegerOps.toDouble(that.getValue());
    }
  }

  @Fallback
  Object doOther(double self, Object that) {
    return incomparableError(self, that);
  }
}
