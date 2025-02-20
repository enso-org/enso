package org.enso.runtime.parser.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A constructor parameter annotated with this annotation will have a corresponding user-defined
 * field generated in the super class (See {@link GenerateFields} for docs about fields).
 *
 * <p>There is no restriction on the type of the parameter.
 */
@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.PARAMETER)
public @interface IRField {
  /** If true, the field will always be non-null. Otherwise, it can be null. */
  boolean required() default true;
}
