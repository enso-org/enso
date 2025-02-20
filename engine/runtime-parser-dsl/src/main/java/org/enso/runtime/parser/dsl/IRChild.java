package org.enso.runtime.parser.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Constructor parameter annotated with this annotation will be represented as a child field in the
 * generated super class. Children of IR elements form a tree. A child will be part of the methods
 * traversing the tree, like {@code mapExpression} and {@code children}. The parameter type must be
 * a subtype of {@code org.enso.compiler.ir.IR}.
 */
@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.PARAMETER)
public @interface IRChild {
  /** If true, the child will always be non-null. Otherwise, it can be null. */
  boolean required() default true;
}
