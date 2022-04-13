package org.enso.interpreter.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** An annotation denoting a node that should be wrapped for standard library export. */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.SOURCE)
public @interface BuiltinType {
    /**
     * Comma-separated list of parameters of builting type
     * @return list of params
     */
    // FIXME:
    String params() default "";
}
