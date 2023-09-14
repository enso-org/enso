package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "/", description = "Division of numbers.")
public abstract class DivideNode extends FloatNode {
  abstract double execute(double self, Object that);

  static DivideNode build() {
    return DivideNodeGen.create();
  }

  @Specialization
  double doDouble(double self, double that) {
    return self / that;
  }

  @Specialization
  double doLong(double self, long that) {
    return self / that;
  }

  @Specialization
  double doBigInteger(double self, EnsoBigInteger that) {
    return self / BigIntegerOps.toDouble(that.getValue());
  }

  @Fallback
  double doOther(double self, Object that) {
    return handleOther(self, that);
  }
}
