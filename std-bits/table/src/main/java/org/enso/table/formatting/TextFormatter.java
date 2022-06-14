package org.enso.table.formatting;

public class TextFormatter implements DataFormatter {
    @Override
    public String format(Object value) {
        if (value == null) {
            return NULL_REPRESENTATION;
        }

        return "TODO";
    }

    @Override
    public boolean canFormat(Object value) {
        return value instanceof String;
    }
}
