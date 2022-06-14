package org.enso.table.formatting;

public class DecimalFormatter implements DataFormatter {

    public String format(double value) {
        // TODO
        return "TODO";
    }

    @Override
    public String format(Object value) {
        if (value == null) {
            return NULL_REPRESENTATION;
        }

        if (value instanceof Double decimal) {
            return format(decimal.doubleValue());
        }

        if (value instanceof Long integer) {
            return format(integer.doubleValue());
        }

        throw new IllegalArgumentException("Unsupported type for DecimalFormatter.");
    }

    @Override
    public boolean canFormat(Object value) {
        return value instanceof Double || value instanceof Long;
    }
}
