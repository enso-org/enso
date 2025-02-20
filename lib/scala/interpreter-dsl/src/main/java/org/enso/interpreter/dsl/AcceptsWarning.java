package org.enso.interpreter.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An interface marking an argument as allowing it to accept a WithWarnings. Parameters annotated
 * with this annotation will not have their warnings stripped, so if it is of type {@link
 * org.enso.interpreter.runtime.warning.WithWarnings} it will not be unwrapped.
 */
@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.SOURCE)
public @interface AcceptsWarning {}
