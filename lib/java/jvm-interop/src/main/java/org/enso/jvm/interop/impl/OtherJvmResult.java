package org.enso.jvm.interop.impl;

/**
 * Interface describing a possible reply from the other JVM.
 *
 * @param <R> the type of result, when the operation succeeds
 * @param <E> the type of exception when the operation fails
 */
public sealed interface OtherJvmResult<R, E extends Exception> // Either R or E
    permits OtherJvmMessage.ReturnValue,
        OtherJvmMessage.ThrowValue,
        OtherJvmMessage.ThrowException {
  /**
   * Either returns the computed result or throws exception.
   *
   * @return the value
   * @throws the exception if value couldn't be computed
   */
  R value() throws E;
}
