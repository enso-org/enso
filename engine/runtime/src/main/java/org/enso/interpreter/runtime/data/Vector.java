package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/** An immutable array-like collection. */
@ExportLibrary(InteropLibrary.class)
public class Vector implements TruffleObject {
  private final @CompilerDirectives.CompilationFinal Object[] items;

  /**
   * Creates a new Vector
   * @param items the element values
   */
  public Vector(Object... items) {
    this.items = items;
  }

  /**
   * Marks the object as array-like for Polyglot APIs.
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
   * @return
   */
  @ExportMessage
  public long getArraySize() {
    return items.length;
  }

  /**
   * Exposes an index validity check through the polyglot API.
   *
   * @param index the index to check
   * @return {@code true} if the index is valid, {@code false} otherwise.
   */
  @ExportMessage
  public boolean isArrayElementReadable(long index) {
    return index < getArraySize() && index >= 0;
  }
}
