package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Integer",
    name = "to_decimal",
    description = "Conversion of integers to decimals.")
public abstract class ToDecimalNode extends IntegerNode {
  public abstract Object execute(Object self);

  public static ToDecimalNode build() {
    return ToDecimalNodeGen.create();
  }

  @Specialization
  double doLong(long self) {
    return self;
  }

  @Specialization
  double doBigInt(EnsoBigInteger self) {
    return BigIntegerOps.toDouble(self.getValue());
  }

  @Fallback
  Object doOther(Object self) {
    throw throwTypeErrorIfNotInt(self);
  }
}
