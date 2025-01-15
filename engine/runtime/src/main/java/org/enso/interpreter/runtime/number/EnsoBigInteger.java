package org.enso.interpreter.runtime.number;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.math.BigInteger;
import org.enso.interpreter.runtime.builtin.BuiltinObject;

/** Internal wrapper for a {@link BigInteger}. */
@ExportLibrary(InteropLibrary.class)
public final class EnsoBigInteger extends BuiltinObject {
  private final BigInteger value;

  /**
   * Wraps a {@link BigInteger}.
   *
   * @param value the value to wrap.
   */
  public EnsoBigInteger(BigInteger value) {
    assert (value.bitLength() > 63) : "Too small BigInteger: " + value;
    this.value = value;
  }

  /**
   * @return the contained {@link BigInteger}.
   */
  public BigInteger getValue() {
    return value;
  }

  @Override
  protected String builtinName() {
    return "Integer";
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return value.toString();
  }

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  @Override
  public String toDisplayString(boolean allowSideEffects) {
    return value.toString();
  }

  @ExportMessage
  boolean isNumber() {
    return true;
  }

  @ExportMessage
  final boolean fitsInByte() {
    return false;
  }

  @ExportMessage
  final boolean fitsInShort() {
    return false;
  }

  @ExportMessage
  final boolean fitsInInt() {
    return false;
  }

  @ExportMessage
  final boolean fitsInLong() {
    return false;
  }

  @ExportMessage
  final boolean fitsInFloat() {
    return false;
  }

  @ExportMessage
  final boolean fitsInDouble() {
    return false;
  }

  @ExportMessage
  final boolean fitsInBigInteger() {
    return true;
  }

  @ExportMessage
  final byte asByte() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  final short asShort() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  final int asInt() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  final long asLong() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  final float asFloat() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  public final double asDouble() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  public final BigInteger asBigInteger() {
    return value;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof EnsoBigInteger otherBigInt) {
      return value.equals(otherBigInt.value);
    } else {
      return false;
    }
  }
}
