package org.enso.interpreter.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An annotation denoting a node that should be wrapped for standard library export. A subclass of
 * {@code BuiltinRootNode} is generated with implementation of {@code
 * InlineableRootNode#createDirectCallNode()} that either delegates to regular {@link
 * DirectCallNode} (when the {@code execute} method requires {@code VirtualFrame} as one of its
 * arguments) or provides a special implementation, if no {@code VirtualFrame} is needed.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.SOURCE)
public @interface BuiltinMethod {
  /** @return the language-level type of {@code self} argument. */
  String type();

  /** @return the language-level name of this method. */
  String name();

  /** @return a short description of this method. */
  String description() default "";

  /** @return a list of aliases (names) of this method */
  String aliases() default "";

  /** @return whether a method should be registered automatically with a type */
  boolean autoRegister() default true;
}
