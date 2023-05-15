package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
        type = "Decimal",
        name = "round_builtin",
        description = "Decimal round, converting to a small or big integer depending on size.")
public abstract class RoundNode extends Node {
    private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

    abstract Object execute(double self, Object that);

    static RoundNode build() {
        return RoundNodeGen.create();
    }

    static private final long MIN_DECIMAL_PLACES = -15;
    static private final long MAX_DECIMAL_PLACES = 15;

    @Specialization
    Object doLong(double self, long that) {
        if (that < MIN_DECIMAL_PLACES || that > MAX_DECIMAL_PLACES) {
            var ctx = EnsoContext.get(this);
            var msg = "Round: decimal_places must be between " +
                      MIN_DECIMAL_PLACES + " and " + MAX_DECIMAL_PLACES +
                      "(inclusive), but was " + that;
            var error =
                    ctx.getBuiltins()
                            .error()
                            .makeUnsupportedArgumentsError(new Object[] {that}, msg);
            return DataflowError.withoutTrace(error, this);
        }

        double rounded = round(self, that);
        if (that <= 0) {
            // Returns integer
            if (fitsProfile.profile(BigIntegerOps.fitsInLong(rounded))) {
                return (long) rounded;
            } else {
                return new EnsoBigInteger(toBigInteger(rounded));
            }
        } else {
            // Returns double
            return rounded;
        }
    }

    @Fallback
    Object doOther(double self, Object that) {
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        var number = builtins.number().getDecimal();
        throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
    }

    @CompilerDirectives.TruffleBoundary
    private static BigInteger toBigInteger(double rounded) {
        return BigDecimal.valueOf(rounded).toBigIntegerExact();
    }

    private static double round(double self, long decimal_places) {
        double scale = Math.pow(10, decimal_places);
        return Math.floor((self * scale) + 0.5) / scale;
    }
}
