package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Decimal",
    name = "ceil",
    description = "Decimal ceiling, converting to a small or big integer depending on size.")
public class CeilNode extends FloatNode {
  private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

  Object execute(double self) {
    double ceil = Math.ceil(self);
    if (fitsProfile.profile(BigIntegerOps.fitsInLong(ceil))) {
      return (long) ceil;
    } else {
      return new EnsoBigInteger(ceil(ceil));
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static BigInteger ceil(double ceil) {
    return BigDecimal.valueOf(ceil).toBigIntegerExact();
  }
}
