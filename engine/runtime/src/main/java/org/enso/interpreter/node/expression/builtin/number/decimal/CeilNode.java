package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigDecimal;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Decimal",
    name = "ceil",
    description = "Decimal ceiling, converting to a small or big integer depending on size.")
public class CeilNode extends Node {
  private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

  Object execute(double self) {
    double ceil = Math.ceil(self);
    if (fitsProfile.profile(BigIntegerOps.fitsInLong(ceil))) {
      return (long) ceil;
    } else {
      return new EnsoBigInteger(BigDecimal.valueOf(ceil).toBigIntegerExact());
    }
  }
}
