package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
final class VectorDouble implements EnsoObject {
  private final double[] storage;

  private VectorDouble(double[] storage) {
    this.storage = storage;
  }

  final Object toArray() {
    return this.storage;
  }

  //
  // messages for the InteropLibrary
  //

  /**
   * Marks the object as array-like for Polyglot APIs.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  long getArraySize() {
    return storage.length;
  }

  /**
   * Handles reading an element by index through the polyglot API.
   *
   * @param index the index to read
   * @return the element value at the provided index
   * @throws InvalidArrayIndexException when the index is out of bounds.
   */
  @ExportMessage
  Object readArrayElement(long index) throws InvalidArrayIndexException {
    try {
      return storage[Math.toIntExact(index)];
    } catch (ArithmeticException | IndexOutOfBoundsException ex) {
      throw InvalidArrayIndexException.create(index);
    }
  }

  static VectorDouble fromArray(double[] arr) {
    return new VectorDouble(arr);
  }

  /**
   * Exposes an index validity check through the polyglot API.
   *
   * @param index the index to check
   * @return {@code true} if the index is valid, {@code false} otherwise.
   */
  @ExportMessage
  boolean isArrayElementReadable(long index) {
    var size = storage.length;
    return index < size && index >= 0;
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return false;
  }

  @ExportMessage
  final void writeArrayElement(long index, Object value)
      throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementRemovable(long index) {
    return false;
  }

  @ExportMessage
  final void removeArrayElement(long index) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  String toDisplayString(boolean allowSideEffects) {
    final InteropLibrary iop = InteropLibrary.getUncached();
    return DisplayArrayUtils.toDisplayString(this, allowSideEffects, iop);
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().vector();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  //
  // methods for TypesLibrary
  //

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().vector();
  }

  @ExportMessage
  boolean hasWarnings() {
    return false;
  }

  @ExportMessage
  Warning[] getWarnings(Node location) throws UnsupportedMessageException {
    return new Warning[0];
  }

  @ExportMessage
  VectorDouble removeWarnings() {
    return this;
  }

  @ExportMessage
  boolean isLimitReached() {
    return false;
  }

  //
  // helper methods
  //

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return toDisplayString(false);
  }
}
