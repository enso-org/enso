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
        type = "Big_Integer",
        name = "round_builtin",
        description = "Integer round, converting to a small or big integer depending on size.")
public abstract class RoundNode extends Node {
    private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

    abstract Object execute(BigInteger self, Object that);

    static RoundNode build() {
        return RoundNodeGen.create();
    }

    static private final long MIN_DECIMAL_PLACES = -15;

    @Specialization
    Object doLong(BigInteger self, long that) {
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

        BigInteger rounded = BigIntegerOps.round(self, that);
        if (fitsProfile.profile(BigIntegerOps.fitsInLong(rounded))) {
            return rounded.longValueExact();
        } else {
            return rounded;
        }
    }

    @Fallback
    Object doOther(BigInteger self, Object that) {
        Builtins builtins = EnsoContext.get(this).getBuiltins();
        var number = builtins.number().getSmallInteger();
        throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
    }
}
