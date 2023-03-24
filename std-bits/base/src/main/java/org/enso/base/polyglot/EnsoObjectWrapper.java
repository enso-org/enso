package org.enso.base.polyglot;

import org.enso.base.ObjectComparator;
import org.enso.base.text.TextFoldingStrategy;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;

public final class EnsoObjectWrapper implements Comparable<EnsoObjectWrapper> {
    /**
     * Folds the value to ensure consistency with Enso's equality.
     * <p>Case-sensitivity of text folding is controlled by {@code textFoldingStrategy}.
     */
    public static Object foldObject(Object value, TextFoldingStrategy textFoldingStrategy) {
        if (value == null) {
            return null;
        }

        if (value instanceof String s) {
            return textFoldingStrategy.fold(s);
        }

        Object numeric = foldNumeric(value);
        if (numeric != null) {
            return numeric;
        }

        if (value instanceof Boolean) {
            return value;
        }

        if (value instanceof LocalDate
                || value instanceof LocalTime
                || value instanceof ZonedDateTime) {
            return value;
        }

        return new EnsoObjectWrapper(value);
    }

    /**
     * If the value is a numeric type, this method coerces it in such a way to ensure consistency with
     * Enso.
     *
     * <p>Integer types are coerced to {@code Long} and floating point values are coerced to {@code
     * Double} unless they represent a whole integer in which case they are also coerced to {@code
     * Long}, to ensure the Enso property that {@code 2 == 2.0}.
     *
     * <p>Returns {@code null} if the value was not a numeric value.
     */
    private static Object foldNumeric(Object value) {
        if (value instanceof Long) {
            return value;
        } else if (value instanceof Integer i) {
            return i.longValue();
        } else if (value instanceof Byte b) {
            return b.longValue();
        } else if (value instanceof Float f && f % 1 == 0) {
            return f.longValue();
        } else if (value instanceof Double d && d % 1 == 0) {
            return d.longValue();
        } else if (value instanceof Float f) {
            return f.doubleValue();
        } else if (value instanceof Double d) {
            return d;
        }

        return null;
    }

    private final Object value;
    private final int ensoHashCode;

    public EnsoObjectWrapper(Object value) {
        this.value = value;
        this.ensoHashCode = ObjectComparator.ensoHashCode(value);
    }

    public Object getValue() {
        return value;
    }

    @Override
    public int hashCode() {
        return ensoHashCode;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof EnsoObjectWrapper that) {
            return ensoHashCode == that.ensoHashCode && ObjectComparator.areEqual(this.value, that.value);
        } else {
            return false;
        }
    }

    @Override
    public int compareTo(EnsoObjectWrapper o) {
        return ObjectComparator.ensoCompare(this.value, o.value);
    }
}
