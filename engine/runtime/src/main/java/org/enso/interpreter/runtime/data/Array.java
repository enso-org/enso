package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.util.Arrays;
import org.enso.interpreter.runtime.error.WithWarnings;

/** A primitive boxed array type for use in the runtime. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Data.Array.Array")
public final class Array implements TruffleObject {
  private final Object[] items;

  /**
   * Creates a new array
   *
   * @param items the element values
   */
  @Builtin.Method(expandVarargs = 4, description = "Creates an array with given elements.", autoRegister = false)
  public Array(Object... items) {
    this.items = items;
  }

  /**
   * Creates an uninitialized array of the given size.
   *
   * @param size the size of the created array.
   */
  @Builtin.Method(description = "Creates an uninitialized array of a given size.", autoRegister = false)
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
    var e = items[(int) index];
    if (e instanceof WithWarnings w) {
      return w.getValue();
    }
    return e;
  }

  public long length() {
    return this.getItems().length;
  }

  /** @return an empty array */
  @Builtin.Method(description = "Creates an empty Array", autoRegister = false)
  public static Object empty() {
    return new Array();
  }

  /** @return an identity array */
  @Builtin.Method(description = "Identity on arrays, implemented for protocol completeness.")
  public Object toArray() {
    return this;
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
  String toDisplayString(boolean b) {
    return toString();
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return Arrays.toString(items);
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().array();
  }
}
