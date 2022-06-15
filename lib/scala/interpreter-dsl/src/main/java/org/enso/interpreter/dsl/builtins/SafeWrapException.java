package org.enso.interpreter.dsl.builtins;

import com.sun.tools.javac.code.Attribute;
import org.apache.commons.lang3.StringUtils;
import org.enso.interpreter.dsl.Builtin;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Wrapper around {@link Builtin.WrapException} annotation with all elements of Class type resolved.
 * extracted
 */
public record SafeWrapException(Attribute.Class from, Attribute.Class to, Boolean passException) {

    private static final String PanicExceptionClassName = "PanicException";

    public SafeWrapException(Attribute.Class from, Attribute.Class to, Attribute.Constant propagate) {
        this(from, to, propagate != null ? (Boolean) propagate.getValue() : false);
    }

    /**
     * Generate a catch-clause that catches `from`, wraps it into `to` Enso type and rethrows the latter
     * @param methodParameters list of all method's parameters, potentially to be applied to `to` constructor
     * @param builtinTypesParameterCounts a map from builtin errors to the number of parameters in their constructors
     * @return Lines representing the (unclosed) catch-clause catching the runtime `from` exception
     */
    List<String> toCatchClause(List<MethodParameter> methodParameters, Map<String, Integer> builtinTypesParameterCounts) {
        String from = fromAttributeToClassName(from(), true);
        String to = fromAttributeToClassName(to(), false);
        if (passException) {
            if (to.equals(PanicExceptionClassName)) {
                return List.of(
                        "  } catch (" + from + " e) {",
                        "    Builtins builtins = Context.get(this).getBuiltins();",
                        "    throw new PanicException(e, this);"
                );
            } else {
                return List.of(
                        "  } catch (" + from + " e) {",
                        "    Builtins builtins = Context.get(this).getBuiltins();",
                        "    throw new PanicException(builtins.error().make" + to + "(e), this);"
                );
            }
        } else {
            int toParamCount = errorParametersCount(to(), builtinTypesParameterCounts);
            List<String> errorParameters =
                    methodParameters
                            .stream()
                            .limit(toParamCount - 1)
                            .flatMap(x -> x.names(Optional.empty()))
                            .collect(Collectors.toList());
            String errorParameterCode = errorParameters.isEmpty() ? "" : ", " + StringUtils.join(errorParameters, ", ");
            return List.of(
                    "  } catch (" + from + " e) {",
                    "    Builtins builtins = Context.get(this).getBuiltins();",
                    "    throw new PanicException(builtins.error().make" + to + "(self" + errorParameterCode + "), this);"
            );
        }
    }

    private String fromAttributeToClassName(Attribute.Class clazz, Boolean fullName) {
        String baseType = clazz.classType.baseType().toString();
        if (fullName) return baseType;
        else {
            String[] clazzElements = baseType.split("\\.");
            if (clazzElements.length == 0) {
                return baseType;
            } else {
                return clazzElements[clazzElements.length - 1];
            }
        }
    }

    private int errorParametersCount(Attribute.Class clazz, Map<String, Integer> builtinTypesParameterCounts) {
        String clazzSimple = fromAttributeToClassName(clazz, false);
        // `this` counts as 1
        return builtinTypesParameterCounts.getOrDefault(clazzSimple, 1);
    }
}
