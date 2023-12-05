package org.enso.interpreter.dsl.builtins;

import java.util.List;
import java.util.Optional;

import javax.lang.model.element.TypeElement;

import org.enso.interpreter.dsl.Builtin;

/**
 * Wrapper around {@link Builtin.WrapException} annotation with all elements of Class type resolved.
 * extracted
 */
public record SafeWrapException(TypeElement from, Optional<TypeElement> to) {

    private static final String PanicExceptionClassName = "PanicException";
    private static final String UnsupportedMessageExceptionClassName = "UnsupportedMessageException";
    private static final String DefaultClassValue = "Class";

    /**
     * Generate a catch-clause that catches `from`, wraps it into `to` Enso type and rethrows the latter
     * @return Lines representing the (unclosed) catch-clause catching the runtime `from` exception
     */
    List<String> toCatchClause() {
        String from = fromAttributeToClassName(from(), true);
        Optional<String> to = to().map(clazz -> fromAttributeToClassName(clazz, false));
        boolean toPanicExcpetion = needsToPanicExceptionConversion(to);
        if (from.equals(UnsupportedMessageExceptionClassName) && toPanicExcpetion) {
            return List.of(
                "  } catch (" + from + " e) {",
                "    throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);"
            );
        } else if (toPanicExcpetion) {
            return List.of(
            "  } catch ("+from+" e) {",
            "    com.oracle.truffle.api.CompilerDirectives.transferToInterpreter();",
            "    throw new PanicException(EnsoContext.get(this).asGuestValue(e), this);"
            );
        } else {
            return List.of(
                "  } catch (" + from + " e) {",
                "    com.oracle.truffle.api.CompilerDirectives.transferToInterpreter();",
                "    EnsoContext ctx = EnsoContext.get(this);",
                "    Builtins builtins = ctx.getBuiltins();",
                "    throw new PanicException(builtins.error().get" + to.get() + "().wrap(ctx, e), this);"
            );
        }
    }

    private boolean needsToPanicExceptionConversion(Optional<String> clazz) {
        return clazz.map(c -> c.equals(PanicExceptionClassName)).orElse(true);
    }

    private String fromAttributeToClassName(TypeElement typeElement, boolean fullName) {
        if (fullName) {
            return typeElement.getQualifiedName().toString();
        } else {
            return typeElement.getSimpleName().toString();
        }
    }
}
