package org.enso.interpreter.test;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.math.BigInteger;

@ExportLibrary(InteropLibrary.class)
final class WrappedPrimitive implements TruffleObject {

  private final Object value;

  WrappedPrimitive(long value) {
    this.value = value;
  }

  WrappedPrimitive(boolean value) {
    this.value = value;
  }

  WrappedPrimitive(double value) {
    this.value = value;
  }

  WrappedPrimitive(BigInteger value) {
    this.value = value;
  }

  WrappedPrimitive(String value) {
    this.value = value;
  }

  @ExportMessage
  boolean isString() {
    return value instanceof String;
  }

  @ExportMessage
  String asString() {
    return (String) value;
  }

  @ExportMessage
  boolean isNumber() {
    return value instanceof Number;
  }

  @ExportMessage
  boolean isBoolean() {
    return value instanceof Boolean;
  }

  @ExportMessage
  boolean asBoolean() {
    return (Boolean) value;
  }

  @ExportMessage
  boolean fitsInByte() {
    return false;
  }

  @ExportMessage
  boolean fitsInShort() {
    return false;
  }

  @ExportMessage
  boolean fitsInInt() {
    return false;
  }

  @ExportMessage
  boolean fitsInLong() {
    return value instanceof Long;
  }

  @ExportMessage
  boolean fitsInFloat() {
    return false;
  }

  @ExportMessage
  boolean fitsInDouble() {
    return value instanceof Double;
  }

  @ExportMessage
  byte asByte() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  short asShort() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  int asInt() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  long asLong() throws UnsupportedMessageException {
    return (Long) value;
  }

  @ExportMessage
  float asFloat() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  double asDouble() throws UnsupportedMessageException {
    return (Double) value;
  }

  @ExportMessage
  boolean fitsInBigInteger() {
    return value instanceof BigInteger;
  }

  @ExportMessage
  BigInteger asBigInteger() throws UnsupportedMessageException {
    return (BigInteger) value;
  }

  @ExportMessage
  String toDisplayString(boolean ignore) {
    return toString();
  }

  Object asDirect() {
    return value;
  }

  @TruffleBoundary
  @Override
  public String toString() {
    return "WrappedPrimitive[" + value + "]";
  }
}
