package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import com.oracle.truffle.api.profiles.PrimitiveValueProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
        type = "Integer",
        name = "round",
        description = "Decimal ceiling, converting to a small or big integer depending on size.")
public class RoundNode extends Node {
    private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

    /** Minimum value for the `decimal_places` parameter to `roundDouble`. */
    private static final double ROUND_MIN_DECIMAL_PLACES = -15;

    /** Maximum value for the `decimal_places` parameter to `roundDouble`. */
    private static final double ROUND_MAX_DECIMAL_PLACES = 15;

    private final PrimitiveValueProfile constantPlacesDecimalPlaces = PrimitiveValueProfile.create();

    private final PrimitiveValueProfile constantPlacesUseBankers = PrimitiveValueProfile.create();

    private final BranchProfile outOfRangeProfile = BranchProfile.create();

    Object execute(long n, long dp, boolean ub) {
        var decimalPlaces = constantPlacesDecimalPlaces.profile(dp);
        var useBankers = constantPlacesUseBankers.profile(ub);

        if (decimalPlaces < ROUND_MIN_DECIMAL_PLACES || decimalPlaces > ROUND_MAX_DECIMAL_PLACES) {
            outOfRangeProfile.enter();
            throw decimalPlacesOutOfRangePanic(decimalPlaces);
        }

        if (decimalPlaces >= 0) {
            return n;
        }

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

    @TruffleBoundary
    private PanicException decimalPlacesOutOfRangePanic(long decimalPlaces) throws PanicException {
        String msg =
                "round: decimalPlaces must be between "
                        + ROUND_MIN_DECIMAL_PLACES
                        + " and "
                        + ROUND_MAX_DECIMAL_PLACES
                        + " (inclusive), but was "
                        + decimalPlaces;
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        throw new PanicException(builtins.error().makeUnsupportedArgumentsError(new Object[] { decimalPlaces }, msg), this);
    }
}
