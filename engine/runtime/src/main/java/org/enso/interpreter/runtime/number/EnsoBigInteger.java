package org.enso.interpreter.runtime.number;

import java.math.BigInteger;

import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** Internal wrapper for a {@link BigInteger}. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class EnsoBigInteger implements TruffleObject {
  private final BigInteger value;

  /**
   * Wraps a {@link BigInteger}.
   *
   * @param value the value to wrap.
   */
  public EnsoBigInteger(BigInteger value) {
    assert (value.bitLength() > 63);
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

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
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
  @CompilerDirectives.TruffleBoundary
  final byte asByte() throws UnsupportedMessageException {
    return value.byteValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final short asShort() throws UnsupportedMessageException {
    return value.shortValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final int asInt() throws UnsupportedMessageException {
    return value.intValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final long asLong() throws UnsupportedMessageException {
    return value.longValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final float asFloat() {
    return value.floatValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  public final double asDouble() {
    return value.doubleValue();
  }

  @ExportMessage
  public final BigInteger asBigInteger() {
    return value;
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().number().getBigInteger();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().number().getBigInteger();
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
