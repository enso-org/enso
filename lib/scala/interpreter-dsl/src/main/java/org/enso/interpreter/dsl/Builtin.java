package org.enso.interpreter.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** An annotation denoting a method that will auto-generate a BuiltinMethod node. */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.SOURCE)
public @interface Builtin {
  /** @return the name of the subpackage for the generated method node. */
  String pkg() default "";

  /** @return a short description of this method. */
  String description() default "";
}
