package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import java.util.Arrays;

/**
 * A primitive boxed array type to be used only in EPB.
 *
 * <p>{@link ReadOnlyArray} is essentially a stripped-down, read-only, version of {@link
 * org.enso.interpreter.runtime.data.Array}, used for passing arguments. The latter cannot be used
 * in EPB because EPB is a dependency of runtime.
 */
@ExportLibrary(InteropLibrary.class)
public class ReadOnlyArray implements TruffleObject {

  private final Object[] items;

  /**
   * Creates a new array
   *
   * @param items the element values
   */
  public ReadOnlyArray(Object... items) {
    this.items = items;
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
   * @return the size of this array
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
    throw new UnsupportedOperationException("writing unsupoorted in PrimArray");
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return false;
  }

  @Override
  public String toString() {
    return Arrays.toString(items);
  }
}
