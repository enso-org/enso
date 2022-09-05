package org.enso.interpreter.dsl.builtins;

import java.util.List;
import java.util.stream.Collectors;

/**
 * TypeWithKind provides a convenience wrapper for the types that can be encountered
 * in builtins construction.
 *
 * For example:
 * - Java's `Foo[]` type is of kind `Array` and base type `Foo`
 * - `void` return type is of kind `Void` and base type `Nothing`
 * - all other accepted types are of kind `Object`
 */
public record TypeWithKind(String baseType, TypeKind kind) {
    static TypeWithKind createFromTpe(String tpeName) {
        if (tpeName.equals("void")) {
            return new TypeWithKind("Nothing", TypeKind.VOID);
        } else if (tpeName.endsWith("[]")) {
            int idx = tpeName.indexOf("[");
            return new TypeWithKind(tpeName.substring(0, idx), TypeKind.ARRAY);
        } else {
            return new TypeWithKind(tpeName, TypeKind.OBJECT);
        }
    }

    // Heuristic
    boolean isValidGuestType() {
        return baseType.startsWith("java.lang") || primitiveTypes.contains(baseType) || validGuestTypes.contains(baseType);
    }

    private final static List<String> primitiveTypes =
            List.of(Boolean.TYPE, Long.TYPE, Double.TYPE, Float.TYPE).stream().map(Class::getSimpleName).collect(Collectors.toList());
    /**
     * A list of hard-coded types that can be used in the parameter or return type position
     * that are valid host types i.e extend TruffleObject.
     * Cannot go via reflection and check that they implement the interface, unfortunately.
     */
    private static List<String> validGuestTypes =
            List.of(
                    "org.enso.interpreter.runtime.callable.atom.Atom",
                    "org.enso.interpreter.runtime.data.Array",
                    "org.enso.interpreter.runtime.data.ArrayOverBuffer",
                    "org.enso.interpreter.runtime.data.EnsoFile",
                    "org.enso.interpreter.runtime.data.EnsoDate",
                    "org.enso.interpreter.runtime.data.EnsoDateTime",
                    "org.enso.interpreter.runtime.data.EnsoTimeOfDay",
                    "org.enso.interpreter.runtime.data.EnsoTimeZone",
                    "org.enso.interpreter.runtime.data.ManagedResource",
                    "org.enso.interpreter.runtime.data.Ref",
                    "org.enso.interpreter.runtime.data.text.Text",
                    "org.enso.interpreter.runtime.error.Warning",
                    "org.enso.interpreter.runtime.error.WithWarnings");

}



