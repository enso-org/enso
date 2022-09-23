package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;

@ExportLibrary(InteropLibrary.class)
class ArraySlice implements TruffleObject {
  private final Object storage;
  private final long start;
  private final long end;

  ArraySlice(Object storage, long start, long end) {
    if (storage instanceof ArraySlice slice) {
      this.storage = slice.storage;
      this.start = slice.start + start;
      this.end = Math.min(slice.end, slice.start + end);
    } else {
      if (CompilerDirectives.inInterpreter()) {
        if (!InteropLibrary.getUncached().hasArrayElements(storage)) {
          throw new IllegalStateException("ArraySlice needs array-like delegate, but got: " + storage);
        }
      }

      this.storage = storage;
      this.start = start;
      this.end = end;
    }
  }

  /**
   * Marks the object as array-like for Polyglot APIs.
   *
   * @return {@code true}
   */
  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  public long getArraySize(@CachedLibrary(limit = "3") InteropLibrary interop)
      throws UnsupportedMessageException {
    long storageSize = interop.getArraySize(storage);
    return Math.max(0, Math.min(storageSize, end) - start);
  }

  /**
   * Handles reading an element by index through the polyglot API.
   *
   * @param index the index to read
   * @return the element value at the provided index
   * @throws InvalidArrayIndexException when the index is out of bounds.
   */
  @ExportMessage
  public Object readArrayElement(
      long index,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @Cached HostValueToEnsoNode toEnso)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    if (index < 0 || index >= getArraySize(interop)) {
      throw InvalidArrayIndexException.create(index);
    }

    var v = interop.readArrayElement(storage, start + index);
    return toEnso.execute(v);
  }

  /**
   * Exposes an index validity check through the polyglot API.
   *
   * @param index the index to check
   * @return {@code true} if the index is valid, {@code false} otherwise.
   */
  @ExportMessage
  boolean isArrayElementReadable(long index, @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      return index >= 0 && index < getArraySize(interop);
    } catch (UnsupportedMessageException e) {
      return false;
    }
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return false;
  }

  @ExportMessage
  final void writeArrayElement(long index, Object value)
      throws UnsupportedMessageException {
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
}
