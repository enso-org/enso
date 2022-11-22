package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Decimal",
    name = "floor",
    description = "Decimal floor, converting to a small or big integer depending on size.")
public class FloorNode extends Node {
  private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

  Object execute(double self) {
    double floor = Math.floor(self);
    if (fitsProfile.profile(BigIntegerOps.fitsInLong(floor))) {
      return (long) floor;
    } else {
      return new EnsoBigInteger(floor(floor));
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static BigInteger floor(double floor) {
    return BigDecimal.valueOf(floor).toBigIntegerExact();
  }
}
