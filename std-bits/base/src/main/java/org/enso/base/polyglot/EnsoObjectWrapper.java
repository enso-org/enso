package org.enso.base.polyglot;

import org.enso.base.ObjectComparator;
import org.graalvm.polyglot.Context;

import java.util.function.BiFunction;
import java.util.function.Function;

public final class EnsoObjectWrapper implements Comparable<EnsoObjectWrapper> {
    private static Function<Object, Integer> ensoHashCodeCallback = null;
    private static BiFunction<Object, Object, Boolean> ensoAreEqualCallback = null;

    private static void initCallbacks() {
        if (ensoHashCodeCallback == null) {
            var module = Context.getCurrent().getBindings("enso").invokeMember("get_module", "Standard.Base.Data.Ordering");
            var type = module.invokeMember("get_type", "Comparable");

            var hash_callback = module.invokeMember("get_method", type, "hash_callback");
            ensoHashCodeCallback = v -> {
                var result = hash_callback.execute(null, v);
                if (result.isNull()) {
                    throw new IllegalStateException("Unable to object hash in EnsoObjectWrapper for " + v.toString());
                } else {
                    return result.asInt();
                }
            };

            var are_equal = module.invokeMember("get_method", type, "compare_callback");
            ensoAreEqualCallback = (v, u) -> {
                var result = are_equal.execute(null, v, u);
                return !result.isNull() && result.asInt() == 0;
            };
        }
    }

    private static int getEnsoHashCode(Object value) {
        initCallbacks();
        return ensoHashCodeCallback.apply(value);
    }

    private static boolean areEqual(Object value, Object other) {
        initCallbacks();
        return ensoAreEqualCallback.apply(value, other);
    }

    private final Object value;
    private final int ensoHashCode;

    public EnsoObjectWrapper(Object value) {
        this.value = value;
        this.ensoHashCode = getEnsoHashCode(value);
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
            return areEqual(this.value, that.value);
        } else {
            return false;
        }
    }

    @Override
    public int compareTo(EnsoObjectWrapper o) {
        return ObjectComparator.ensoCompare(this.value, o.value);
    }
}
