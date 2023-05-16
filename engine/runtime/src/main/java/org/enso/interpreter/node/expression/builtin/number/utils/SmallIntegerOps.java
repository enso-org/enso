package org.enso.interpreter.node.expression.builtin.number.utils;

import com.oracle.truffle.api.CompilerDirectives;

/** Re-exposes small-integer operations behind a truffle boundary. */
public class SmallIntegerOps {
    // This assumes decimal places is between -15 and 15 (inclusive). The caller
    // must ensure this.
    @CompilerDirectives.TruffleBoundary
    public static BigInteger round(long a, long decimal_places) {
        double scale = Math.pow(10, decimal_places);
        return (long) (((a * scale) + 0.5).floor() / scale);
    }
}
