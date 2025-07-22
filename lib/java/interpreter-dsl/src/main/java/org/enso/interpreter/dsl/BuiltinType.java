package org.enso.interpreter.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** An annotation denoting a node that should be wrapped for standard library export. */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.SOURCE)
public @interface BuiltinType {

  /** Fully qualified name as available in stdlib */
  String name() default "";

  /** Underlying type name of the builtin */
  String underlyingTypeName() default "";
}
