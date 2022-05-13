package org.enso.interpreter.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** An annotation denoting a method that will auto-generate a BuiltinMethod node. */
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.TYPE})
@Retention(RetentionPolicy.SOURCE)
public @interface Builtin {
  /** @return the name of the subpackage for the generated method node. */
  String pkg() default "";

  /** @return a custom name, by default it uses the name of the annotated element. */
  String name() default "";

  /** @return a short description of this method. */
  String description() default "";

  /**
   * @return when applied to a method/constructor with varargs, will generate methods with
   *     parameters repeated up to the value. Must be zero when there are no varaargs in the
   *     parameters list.
   */
  int expandVarargs() default 0;

  /**
   * @return even-length array representing pairs of classes. The first element of the tuple always represents
   * a possible runtime exception that can be thrown during the execution of the method, while the second element
   * represents Enso builtin type wrapper for it that will be reported to the user.
   */
  Class[] wrapException() default {};
}
