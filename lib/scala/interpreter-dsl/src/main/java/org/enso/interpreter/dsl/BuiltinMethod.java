package org.enso.interpreter.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An annotation denoting a node that should be wrapped for standard library export. A subclass of
 * {@code BuiltinRootNode} is generated with implementation of {@code
 * InlineableNode.Root.createInlineableNode()} that either delegates to regular {@link
 * DirectCallNode} or provides a special and faster implementation depending on implicit or explicit
 * value of {@link #inlineable()} attribute.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.SOURCE)
public @interface BuiltinMethod {
  /**
   * @return the language-level type of {@code self} argument.
   */
  String type();

  /**
   * @return the language-level name of this method.
   */
  String name();

  /**
   * @return a short description of this method.
   */
  String description() default "";

  /**
   * @return a list of aliases (names) of this method
   */
  String aliases() default "";

  /**
   * @return whether a method should be registered automatically with a type
   */
  boolean autoRegister() default true;

  /**
   * Allow aggressive inlining or not. This argument doesn't need to be specified. If it is missing,
   * its <em>effective value</em> is derived from the arguments of the annotated method. When the
   * {@code execute} method requires {@code VirtualFrame} as one of its arguments the value of
   * unspecified {@link #inlineable()} is {@code false}. When no {@code VirtualFrame} is needed, the
   * value is assumed to be {@code true}.
   *
   * @return explicitly specify whether the builtin can share {@link VirtualFrame} with its caller.
   */
  boolean inlineable() default true;
}
