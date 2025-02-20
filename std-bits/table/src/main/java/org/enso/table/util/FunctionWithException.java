package org.enso.table.util;

import java.util.Objects;
import java.util.function.Function;

/**
 * Same as {@link Function} except that a one can declare a checked exception, E. Represents a
 * function that accepts one argument and produces a result.
 *
 * <p>This is a <a href="package-summary.html">functional interface</a> whose functional method is
 * {@link #apply(Object)}.
 *
 * @param <T> the type of the input to the function
 * @param <R> the type of the result of the function
 * @param <E> the type of the checked exception
 */
@FunctionalInterface
public interface FunctionWithException<T, R, E extends Throwable> {

  /**
   * Applies this function to the given argument.
   *
   * @param t the function argument
   * @return the function result
   */
  R apply(T t) throws E;

  default <V> FunctionWithException<V, R, E> compose(
      FunctionWithException<? super V, ? extends T, E> before) {
    Objects.requireNonNull(before);
    return (V v) -> apply(before.apply(v));
  }

  /**
   * Returns a composed function that first applies this function to its input, and then applies the
   * {@code after} function to the result. If evaluation of either function throws an exception, it
   * is relayed to the caller of the composed function.
   *
   * @param <V> the type of output of the {@code after} function, and of the composed function
   * @param after the function to apply after this function is applied
   * @return a composed function that first applies this function and then applies the {@code after}
   *     function
   * @throws NullPointerException if after is null
   * @see #compose(Function)
   */
  default <V> FunctionWithException<T, V, E> andThen(
      FunctionWithException<? super R, ? extends V, E> after) {
    Objects.requireNonNull(after);
    return (T t) -> after.apply(apply(t));
  }
}
