package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import com.oracle.truffle.api.profiles.ValueProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
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

    private final ValueProfile constantPlacesDecimalPlaces = ValueProfile.createIdentityProfile();

    private final ValueProfile constantPlacesUseBankers = ValueProfile.createIdentityProfile();

    Object execute(double n, long decimalPlaces, boolean useBankers) {
        decimalPlaces = constantPlacesDecimalPlaces.profile(decimalPlaces);
        useBankers = constantPlacesUseBankers.profile(useBankers);

        if (decimalPlaces < ROUND_MIN_DECIMAL_PLACES || decimalPlaces > ROUND_MAX_DECIMAL_PLACES) {
            decimalPlacesOutOfRangePanic(decimalPlaces);
        }
        if (Double.isNaN(n) || Double.isInfinite(n)) {
            specialValuePanic(n);
        }
        if (n < ROUND_MIN_DOUBLE || n > ROUND_MAX_DOUBLE) {
            argumentOutOfRangePanic(n);
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

    @TruffleBoundary
    private void decimalPlacesOutOfRangePanic(long decimalPlaces) throws PanicException {
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

    @TruffleBoundary
    private void argumentOutOfRangePanic(double n) throws PanicException {
        String msg =
                "Error: `round` can only accept values between "
                        + ROUND_MIN_DOUBLE
                        + " and "
                        + ROUND_MAX_DOUBLE
                        + " (inclusive), but was "
                        + n;
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        throw new PanicException(builtins.error().makeUnsupportedArgumentsError(new Object[] { n }, msg), this);
    }

    @TruffleBoundary
    private void specialValuePanic(double n) throws PanicException {
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        throw new PanicException(builtins.error().getDecimalPlacesTooBigError(), this);
    }
}
