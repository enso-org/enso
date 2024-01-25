package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import com.oracle.truffle.api.profiles.PrimitiveValueProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.RoundHelpers;

@BuiltinMethod(
    type = "Integer",
    name = "round",
    description = "Decimal ceiling, converting to a small or big integer depending on size.")
public class RoundNode extends IntegerNode {
  private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

  private final PrimitiveValueProfile constantPlacesDecimalPlaces = PrimitiveValueProfile.create();

  private final PrimitiveValueProfile constantPlacesUseBankers = PrimitiveValueProfile.create();

  private final BranchProfile decimalPlacesOutOfRangeProfile = BranchProfile.create();

  Object execute(long n, long dp, boolean ub) {
    var decimalPlaces = constantPlacesDecimalPlaces.profile(dp);

    // We don't check if `n` is out of range here, since the Enso wrapper does.
    if (decimalPlaces < RoundHelpers.ROUND_MIN_DECIMAL_PLACES
        || decimalPlaces > RoundHelpers.ROUND_MAX_DECIMAL_PLACES) {
      decimalPlacesOutOfRangeProfile.enter();
      RoundHelpers.decimalPlacesOutOfRangePanic(this, decimalPlaces);
    }

    if (decimalPlaces >= 0) {
      return n;
    }

    var useBankers = constantPlacesUseBankers.profile(ub);
    long scale = (long) Math.pow(10, -decimalPlaces);
    long halfway = scale / 2;
    long remainder = n % scale;
    long scaledDown = n / scale;
    long resultUnnudged = scaledDown * scale;

    if (n >= 0) {
      boolean halfGoesUp = useBankers ? (scaledDown % 2) != 0 : true;
      boolean roundUp = halfGoesUp ? remainder >= halfway : remainder > halfway;
      return roundUp ? resultUnnudged + scale : resultUnnudged;
    } else {
      boolean halfGoesUp = useBankers ? (scaledDown % 2) == 0 : false;
      boolean roundUp = halfGoesUp ? remainder < -halfway : remainder <= -halfway;
      return roundUp ? resultUnnudged - scale : resultUnnudged;
    }
  }
}
