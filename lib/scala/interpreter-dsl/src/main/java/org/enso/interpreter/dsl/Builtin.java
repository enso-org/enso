package org.enso.interpreter.dsl;

import java.lang.annotation.*;

/** An annotation denoting a method that will auto-generate a BuiltinMethod node. */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.SOURCE)
public @interface Builtin {
  /** @return the name of the subpackage for the generated method node. */
  String pkg() default "";

  /** @return A fully qualified name of the corresponding Enso type in standard library. */
  String stdlibName() default "";

  String name() default "";

  @Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
  @interface Method {
    /**
     * @return a custom name for the generated builting method. By default, it uses the name of the
     *     annotated method.
     */
    String name() default "";

    /** @return a short description for the generated builtin method. */
    String description() default "";

    /**
     * @return When applied to a method/constructor with varargs, indicates how many times vararg
     *     parameter should be expanded and implicitly how many builtin nodes should be generated.
     *     Must be zero when there are no varaargs in the parameters list.
     */
    int expandVarargs() default 0;
  }

  /**
   * Annotation indicating that a potential exception during the execution of the method should be
   * wrapped in Enso's error type that will be reported to the user. The annotation can be repeated
   * leading to multiple catch clauses.
   */
  @Repeatable(WrapExceptions.class)
  @interface WrapException {
    /** @return Class of the potential exception to be caught during the execution of the method */
    Class<? extends Exception> from();
    /** @return Class of Enso's builtin (error) type */
    Class<?> to();

    /**
     * @return true, if only the original exception should be wrapped. Otherwise we pass provided
     *     method params.
     */
    boolean propagate() default false;
  }

  /** Container for {@link WrapException} annotations */
  @interface WrapExceptions {
    WrapException[] value() default {};
  }

  /**
   * Annotation accepting `env.asGUestValue` translation done on the return object. The conversion
   * is generated automatically. The annotation only ensures that it is intentianal.
   */
  @interface ReturningGuestObject {}

  /**
   * Method marked with @Builtin.Specialize annotation should also generate specializations for
   * overloaded and non-overloaded methods. The processor infers the parameter on which the
   * specialization should occur and fails if it cannot make that choice in a deterministic way.
   */
  @interface Specialize {}
}
