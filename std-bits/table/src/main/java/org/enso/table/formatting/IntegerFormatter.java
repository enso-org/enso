package org.enso.table.formatting;

import java.math.BigInteger;

public class IntegerFormatter implements DataFormatter {
    public String format(long value) {
        // TODO
        return "TODO";
    }

    public String format(BigInteger value) {
        // TODO
        return "TODO";
    }

    @Override
    public String format(Object value) {
        if (value == null) {
            return NULL_REPRESENTATION;
        }

        if (value instanceof Long integer) {
            return format(integer.longValue());
        }

        if (value instanceof BigInteger bigInteger) {
            return format(bigInteger);
        }

        throw new IllegalArgumentException("Unsupported type for DecimalFormatter.");
    }

    @Override
    public boolean canFormat(Object value) {
        return value instanceof Long || value instanceof BigInteger;
    }
}
