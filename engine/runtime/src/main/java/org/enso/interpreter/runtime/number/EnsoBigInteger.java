package org.enso.interpreter.runtime.number;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.math.BigInteger;

/** Internal wrapper for a {@link BigInteger}. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class EnsoBigInteger extends Number implements TruffleObject {
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
  @CompilerDirectives.TruffleBoundary
  final byte asByte() throws UnsupportedMessageException {
    return byteValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final short asShort() throws UnsupportedMessageException {
    return shortValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final int asInt() throws UnsupportedMessageException {
    return intValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final long asLong() throws UnsupportedMessageException {
    return longValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final float asFloat() throws UnsupportedMessageException {
    return floatValue();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  final double asDouble() throws UnsupportedMessageException {
    return doubleValue();
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().number().getBigInteger();
  }

  @Override
  public int intValue() {
    return value.intValue();
  }

  @Override
  public long longValue() {
    return value.longValue();
  }

  @Override
  public float floatValue() {
    return value.floatValue();
  }

  @Override
  public double doubleValue() {
    return value.doubleValue();
  }
}
