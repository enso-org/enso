package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.SmallIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
        type = "Small_Integer",
        name = "round_builtin",
        description = "Integer round, converting to a small or big integer depending on size.")
public abstract class RoundNode extends Node {
    private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

    private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

    abstract Object execute(long self, Object that);

    static RoundNode build() {
        return RoundNodeGen.create();
    }

    static private final long MIN_DECIMAL_PLACES = -15;
    static private final long MAX_DECIMAL_PLACES = 15;

    // We will use either Double or BigDecimal for intermediate values. For
    // integers 15 digits or less, using Double will have no loss of precision.
    // More than 15 digits will lose precision.
    // (See https://en.wikipedia.org/wiki/Double-precision_floating-point_format.)
    static private final long MIN_LONG_FOR_FAST_PATH = -999_999_999_999_999L;
    static private final long MAX_LONG_FOR_FAST_PATH = 999_999_999_999_999L;

    @Specialization
    Object doLong(long self, long that) {
        if (that >= 0) {
            return self;
        }

        // We don't need to check (that > MAX_DECIMAL_PLACES) because we
        // already know that (that < 0).
        if (that < MIN_DECIMAL_PLACES) {
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

        if (self < MIN_LONG_FOR_FAST_PATH || self > MAX_LONG_FOR_FAST_PATH) {
            BigInteger rounded = BigIntegerOps.round(BigInteger.valueOf(self), that);
            return toEnsoNumberNode.execute(rounded);
        } else {
            return SmallIntegerOps.round(self, that);
        }
    }

    @Fallback
    Object doOther(long self, Object that) {
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        var number = builtins.number().getSmallInteger();
        throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
    }
}
