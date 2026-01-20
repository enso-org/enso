package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.nodes.Node;

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
   * @param location who's querying?
   * @return the value
   * @throws the exception if value couldn't be computed
   */
  R value(Node location) throws E;
}
