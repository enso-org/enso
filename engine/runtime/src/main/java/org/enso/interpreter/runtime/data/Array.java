package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** A primitve boxed array type for use in the runtime. */
@ExportLibrary(InteropLibrary.class)
public class Array implements TruffleObject {
  private final Object[] items;

  /**
   * Creates a new array
   *
   * @param items the element values
   */
  public Array(Object... items) {
    this.items = items;
  }

  /**
   * Creates an uninitialized array of the given size.
   *
   * @param size the size of the created array.
   */
  public Array(long size) {
    this.items = new Object[(int) size];
  }

  /** @return the elements of this array as a java array. */
  public Object[] getItems() {
    return items;
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

  /**
   * Handles reading an element by index through the polyglot API.
   *
   * @param index the index to read
   * @return the element value at the provided index
   * @throws InvalidArrayIndexException when the index is out of bounds.
   */
  @ExportMessage
  public Object readArrayElement(long index) throws InvalidArrayIndexException {
    if (index >= items.length || index < 0) {
      throw InvalidArrayIndexException.create(index);
    }
    return items[(int) index];
  }

  /**
   * Exposes the size of this collection through the polyglot API.
   *
   * @return
   */
  @ExportMessage
  long getArraySize() {
    return items.length;
  }

  /**
   * Exposes an index validity check through the polyglot API.
   *
   * @param index the index to check
   * @return {@code true} if the index is valid, {@code false} otherwise.
   */
  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index < getArraySize() && index >= 0;
  }

  @ExportMessage
  void writeArrayElement(long index, Object value) {
    items[(int) index] = value;
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return isArrayElementReadable(index);
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return false;
  }
}
