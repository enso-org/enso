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
    name = "truncate_builtin",
    description = "Truncate a floating-point number to an integer by dropping the fractional part.")
public class TruncateNode extends FloatNode {
  private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

  Object execute(double self) {
    if (fitsProfile.profile(BigIntegerOps.fitsInLong(self))) {
      return (long) self;
    } else {
      return new EnsoBigInteger(toBigInteger(self));
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static BigInteger toBigInteger(double self) {
    return BigDecimal.valueOf(self).toBigIntegerExact();
  }
}
