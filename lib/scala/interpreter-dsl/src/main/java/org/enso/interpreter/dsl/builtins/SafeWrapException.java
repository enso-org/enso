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
public record SafeWrapException(Attribute.Class from, Attribute.Class to) {

    private static final String PanicExceptionClassName = "PanicException";

    /**
     * Generate a catch-clause that catches `from`, wraps it into `to` Enso type and rethrows the latter
     * @return Lines representing the (unclosed) catch-clause catching the runtime `from` exception
     */
    List<String> toCatchClause() {
        String from = fromAttributeToClassName(from(), true);
        String to = fromAttributeToClassName(to(), false);
        if (to.equals(PanicExceptionClassName)) {
            return List.of(
                "  } catch (" + from + " e) {",
                "    com.oracle.truffle.api.CompilerDirectives.transferToInterpreter();",
                "    Builtins builtins = EnsoContext.get(this).getBuiltins();",
                "    throw new PanicException(e.getMessage(), this);"
            );
        } else {
            return List.of(
                "  } catch (" + from + " e) {",
                "    com.oracle.truffle.api.CompilerDirectives.transferToInterpreter();",
                "    EnsoContext ctx = EnsoContext.get(this);",
                "    Builtins builtins = ctx.getBuiltins();",
                "    throw new PanicException(builtins.error().get" + to + "().wrap(ctx, e), this);"
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
}
