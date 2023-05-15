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
        type = "Decmial",
        name = "int_builtin",
        description = "Truncate a floating-point number to an integer by dropping the fractional part.")
public abstract class IntNode extends Node {
    private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

    abstract Object execute(double self);

    static IntNode build() {
        return IntNodeGen.create();
    }

    @Specialization
    Object doLong(double self) {
        // Returns integer
        if (fitsProfile.profile(BigIntegerOps.fitsInLong(self))) {
            return (long) self;
        } else {
            return new EnsoBigInteger(toBigInteger(self));
        }
    }

    @CompilerDirectives.TruffleBoundary
    private static BigInteger toBigInteger(double self) {
        return BigDecimal.valueOf(self).toBigIntegerExact();
    }
}
