package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

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

    Object execute(long n, long decimalPlaces, boolean useBankers) {
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
}
