package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import com.oracle.truffle.api.profiles.PrimitiveValueProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.RoundHelpers;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Decimal",
    name = "round",
    description = "Decimal ceiling, converting to a small or big integer depending on size.")
public class RoundNode extends FloatNode {
  private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

  private final PrimitiveValueProfile constantPlacesDecimalPlaces = PrimitiveValueProfile.create();

  private final PrimitiveValueProfile constantPlacesUseBankers = PrimitiveValueProfile.create();

  private final BranchProfile decimalPlacesOutOfRangeProfile = BranchProfile.create();

  private final BranchProfile outOfRangeProfile = BranchProfile.create();

  Object execute(double n, long dp, boolean ub) {
    long decimalPlaces = constantPlacesDecimalPlaces.profile(dp);
    boolean useBankers = constantPlacesUseBankers.profile(ub);

    if (decimalPlaces < RoundHelpers.ROUND_MIN_DECIMAL_PLACES
        || decimalPlaces > RoundHelpers.ROUND_MAX_DECIMAL_PLACES) {
      decimalPlacesOutOfRangeProfile.enter();
      RoundHelpers.decimalPlacesOutOfRangePanic(this, decimalPlaces);
    }

    boolean inRange = n >= RoundHelpers.ROUND_MIN_DOUBLE && n <= RoundHelpers.ROUND_MAX_DOUBLE;
    if (!inRange) {
      outOfRangeProfile.enter();
      if (Double.isNaN(n) || Double.isInfinite(n)) {
        RoundHelpers.specialValuePanic(this, n);
      } else {
        RoundHelpers.argumentOutOfRangePanic(this, n);
      }
    }

    // Algorithm taken from https://stackoverflow.com/a/7211688.
    double scale = Math.pow(10.0, decimalPlaces);
    double scaled = n * scale;
    double roundBase = Math.floor(scaled);
    double roundMidpoint = (roundBase + 0.5) / scale;
    boolean evenIsUp = n >= 0 ? (((long) scaled) % 2) != 0 : (((long) scaled) % 2) == 0;
    boolean halfGoesUp = useBankers ? evenIsUp : n >= 0;
    boolean doRoundUp = halfGoesUp ? n >= roundMidpoint : n > roundMidpoint;
    double resultUncast = doRoundUp ? ((roundBase + 1.0) / scale) : (roundBase / scale);
    if (decimalPlaces > 0) {
      return resultUncast;
    } else {
      if (fitsProfile.profile(BigIntegerOps.fitsInLong(resultUncast))) {
        return (long) resultUncast;
      } else {
        return new EnsoBigInteger(toBigInteger(resultUncast));
      }
    }
  }

  @TruffleBoundary
  private BigInteger toBigInteger(double n) {
    return BigDecimal.valueOf(n).toBigIntegerExact();
  }
}
