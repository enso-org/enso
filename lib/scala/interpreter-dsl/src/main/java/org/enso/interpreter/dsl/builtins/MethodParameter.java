package org.enso.interpreter.dsl.builtins;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * MethodParameter encapsulates the generation of string representation of the parameter.
 * Additionally, it can optionally expand vararg parameters.
 */
public record MethodParameter(int index, String name, String tpe, List<String> annotations) {
    /**
     * Returns a parameter's declaration, consisting of its type and name.
     * If the parameter represents a vararg, the declaration is repeated. Otherwise the method
     * returns a single element Stream of the parameter declaration for the method.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter, potentially repeated for varargs
     */
    public Stream<String> declaredParameters(Optional<Integer> expand) {
        // If the parameter is the expanded vararg we must get rid of the `[]` suffix
        TypeWithKind tpeWithKind = TypeWithKind.createFromTpe(tpe);
        String paramTpe;

        switch (tpeWithKind.kind()) {
            case ARRAY:
                // Expanded varargs are no longer represented as Arrays but as individual elements
                if (expand.isEmpty()) {
                    paramTpe = "Array";
                } else {
                    paramTpe = "Object";
                }
                break;
            case OBJECT:
                paramTpe = tpeWithKind.baseType();
                break;
            default:
                throw new RuntimeException("Invalid type for parameter " + name);
        }
        String paramAnnotations = annotations.isEmpty() ? "" : (StringUtils.joinWith(" ", annotations.toArray()) + " ");
        return names(expand).map(n -> paramAnnotations + paramTpe + " " + n);
    }

    /**
     * Parameter's name, capitalized.
     *
     * @return Capitalized parameter's name.
     */
    public String capitalizedName() {
        return this.name.substring(0,1).toUpperCase() + this.name.substring(1);
    }

    /**
     * Determines if the parameter guest object needs to be translated to host representation.
     *
     * @return true, if it needs, false otherwise.
     */
    public boolean needsToHostTranslation() {
        TypeWithKind tpeWithKind = TypeWithKind.createFromTpe(tpe);
        switch (tpeWithKind.kind()) {
            case ARRAY:
                return !tpeWithKind.isValidGuestType();
            default:
                return false;
        }
    }

    /**
     * Name of the variable to store host representation of polyglot object coming from the parameter.
     * See {@link MethodParameter#needsToHostTranslation()}.
     *
     * @return name of the variable that stores host representation of the polyglot object.
     */
    public String hostVarName() {
        return "host" + capitalizedName();
    }

    /**
     * Returns a parameter's name.
     * If the parameter represents a vararg, the name is repeated. Otherwise return a single
     * element Stream of declarations.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter variable, potentially repeated for varargs
     */
    public Stream<String> names(Optional<Integer> expand) {
        return expand.map(e->
                IntStream.range(0, e).mapToObj(i-> name + "_" + (i+1))
        ).orElse(Stream.of(name));
    }

    /**
     * Returns a parameter's name when used in the body of the method.
     * Compared to {@link MethodParameter#names} it will take into account potential
     * translation to Java host representation.
     *
     * @param expand For a non-empty value n, the parameter must be repeated n-times.
     * @return A string representation of the parameter variable, potetnially repeated for varargs
     */
    public Stream<String> paramUseNames(Optional<Integer> expand) {
        if (needsToHostTranslation()) {
            return Stream.of(hostVarName());
        } else {
            return expand.map(e->
                    IntStream.range(0, e).mapToObj(i-> name + "_" + (i+1))
            ).orElse(Stream.of(name));
        }
    }

    public String tpeSimpleName() {
        String[] tpeElements = tpe.split("\\.");
        return tpeElements[tpeElements.length - 1];
    }

    private final static String TruffleDSLPkgAnnotation = "@com.oracle.truffle.api.dsl";

    /**
     * Check if the parameter contains a Truffle-DSL annotation.
     * Parameters annotated with such annotations should not appear in the abstract `execute` method, but
     * should be passed to the specialized method signatures.
     * @return
     */
    public boolean isTruffleInjectedParam() {
        return !annotations.stream().filter(a -> a.startsWith(TruffleDSLPkgAnnotation)).findAny().isEmpty();
    }

    public boolean isTruffleCachedParam() {
        return tpe.startsWith(CachedLibraryMethodParameter.INTEROP_LIBRARY) ||
        tpe.startsWith(CachedLibraryMethodParameter.WARNINGS_LIBRARY);
    }


    public boolean needsToInjectValueOfType() {
        return typesToInject.contains(tpe);
    }

    private static final List<String> typesToInject = List.of("org.enso.interpreter.runtime.EnsoContext");

}
