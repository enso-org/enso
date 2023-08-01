package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
        type = "Decimal",
        name = "round",
        description = "Decimal ceiling, converting to a small or big integer depending on size.")
public class RoundNode extends Node {
    private final CountingConditionProfile fitsProfile = CountingConditionProfile.create();

    /** Minimum value for the `decimal_places` parameter to `roundDouble`. */
    private static final double ROUND_MIN_DECIMAL_PLACES = -15;

    /** Maximum value for the `decimal_places` parameter to `roundDouble`. */
    private static final double ROUND_MAX_DECIMAL_PLACES = 15;

    /** Minimum value for the `n` parameter to `roundDouble`. */
    private static final double ROUND_MIN_DOUBLE = -99999999999999.0;

    /** Minimum value for the `n` parameter to `roundDouble`. */
    private static final double ROUND_MAX_DOUBLE = 99999999999999.0;

    Object execute(double n, long decimalPlaces, boolean useBankers) {
        if (decimalPlaces < ROUND_MIN_DECIMAL_PLACES || decimalPlaces > ROUND_MAX_DECIMAL_PLACES) {
            String msg =
                    "round: decimalPlaces must be between "
                            + ROUND_MIN_DECIMAL_PLACES
                            + " and "
                            + ROUND_MAX_DECIMAL_PLACES
                            + " (inclusive), but was "
                            + decimalPlaces;
            throw new IllegalArgumentException(msg);
        }
        if (Double.isNaN(n) || Double.isInfinite(n)) {
            String msg = "round cannot accept " + n;
            throw new ArithmeticException(msg);
        }
        if (n < ROUND_MIN_DOUBLE || n > ROUND_MAX_DOUBLE) {
            String msg =
                    "Error: `round` can only accept values between "
                            + ROUND_MIN_DOUBLE
                            + " and "
                            + ROUND_MAX_DOUBLE
                            + " (inclusive), but was "
                            + n;
            throw new IllegalArgumentException(msg);
        }

        // Algorithm taken from https://stackoverflow.com/a/7211688.
        double scale = Math.pow(10.0, decimalPlaces);
        double scaled = n * scale;
        double roundBase = Math.floor(scaled);
        double roundMidpoint = (roundBase + 0.5) / scale;
        boolean evenIsUp = n >= 0 ? (((long) scaled) % 2) != 0 : (((long) scaled) % 2) == 0;
        boolean halfGoesUp = useBankers ? evenIsUp : n >= 0;
        boolean doRoundUp = halfGoesUp ? n >= roundMidpoint : n > roundMidpoint;
        double result_uncast = doRoundUp ? ((roundBase + 1.0) / scale) : (roundBase / scale);
        if (decimalPlaces > 0) {
                return result_uncast;
        } else {
            if (fitsProfile.profile(BigIntegerOps.fitsInLong(result_uncast))) {
                return (long) result_uncast;
            } else {
                return new EnsoBigInteger(BigDecimal.valueOf(result_uncast).toBigIntegerExact());
            }
        }
    }
}
