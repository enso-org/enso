package org.enso.interpreter.runtime.number;

import com.oracle.truffle.api.CompilerDirectives;

import java.math.BigInteger;

/** Internal wrapper for a {@link BigInteger}. */
public class EnsoBigInteger {
  private final BigInteger value;

  /**
   * Wraps a {@link BigInteger}.
   *
   * @param value the value to wrap.
   */
  public EnsoBigInteger(BigInteger value) {
    this.value = value;
  }

  /** @return the contained {@link BigInteger}. */
  public BigInteger getValue() {
    return value;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return value.toString();
  }
}
