package org.enso.runtime.parser.dsl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Constructor parameter annotated with this annotation will be represented as a child field in the
 * generated super class, and will have a generated getter method with the same name. Children of IR
 * elements form a tree. A child will be part of the methods traversing the tree, like {@code
 * mapExpression} and {@code children}.
 *
 * <p>The body of the generated getter method for the field depends on the type of the parameter.
 * The return type of the getter method is generally the same as the type of the parameter, with
 * some exceptions documented below.
 *
 * <p>The parameter types must be one of the following:
 *
 * <ul>
 *   <li>Subtype of {@code org.enso.compiler.core.IR}
 *   <li>{@link scala.collection.immutable.List} with type parameter that extends {@code
 *       org.enso.compiler.core.IR}
 *   <li>{@link scala.Option} with type parameter that extends {@code org.enso.compiler.core.IR}
 *   <li>{@link scala.Option} with {@link scala.collection.immutable.List} with type parameter that
 *       extends {@code org.enso.compiler.core.IR}.
 *   <li>{@code org.enso.persist.Persistance.Reference} with type parameter that extends {@code
 *       org.enso.compiler.core.IR}
 * </ul>
 *
 * <p>If the parameter type is {@code org.enso.persist.Persistance.Reference<T>}, then the return
 * type of the getter method is {@code T}. Otherwise, the return type is the same as the parameter
 * type.
 *
 * @see GenerateFields
 */
@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.PARAMETER)
public @interface IRChild {
  /**
   * If true, the child will always be non-null. Otherwise, it can be null. Children of types {@link
   * scala.Option} or {@link scala.collection.immutable.List} are required by default. It is an
   * error to set this to false for a child of type {@link scala.Option} or {@link
   * scala.collection.immutable.List}.
   */
  boolean required() default true;
}
