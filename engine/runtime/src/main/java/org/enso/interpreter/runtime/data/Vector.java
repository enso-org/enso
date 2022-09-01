package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Data.Vector.Vector")
public class Vector implements TruffleObject {
  private final Object storage;

  public Vector(Object storage) {
    this.storage = storage;
  }

  @Builtin.Method(
      name = "new_builtin",
      description = "Creates new Vector with given length and provided elements.")
  @Builtin.Specialize
  public static Object newFromFunction(long length, Function fun, InteropLibrary interop) {
    Object[] target = new Object[(int) length];
    for (int i = 0; i < length; i++) {
      try {
        final Object value = interop.execute(fun, (long) i);
        if (value instanceof DataflowError) {
          return value;
        }
        target[i] = value;
      } catch (ArityException | UnsupportedTypeException | UnsupportedMessageException e) {
        throw raise(RuntimeException.class, e);
      }
    }
    return new Vector(new Array(target));
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
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
   * Exposes the size of this collection through the polyglot API.
   *
   * @return the size of this array
   */
  @ExportMessage
  public long getArraySize() throws UnsupportedMessageException {
    InteropLibrary interop = InteropLibrary.getUncached();
    return interop.getArraySize(storage);
  }

  @Builtin.Method(description = "Returns the length of this Vector.")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class, to = PolyglotError.class)
  public long length(InteropLibrary interop) throws UnsupportedMessageException {
    return interop.getArraySize(storage);
  }

  /**
   * Handles reading an element by index through the polyglot API.
   *
   * @param index the index to read
   * @return the element value at the provided index
   * @throws InvalidArrayIndexException when the index is out of bounds.
   */
  @ExportMessage
  public Object readArrayElement(long index)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    InteropLibrary interop = InteropLibrary.getUncached();
    return interop.readArrayElement(storage, index);
  }

  public boolean isArray() {
    return storage instanceof Array;
  }

  public Array toArrayUnsafe() throws UnsupportedMessageException {
    if (!isArray()) throw UnsupportedMessageException.create();
    return (Array) storage;
  }

  @Builtin.Method(description = "Returns an Array representation of this Vector.")
  public static Vector fromArray(Array arr) {
    return new Vector(arr);
  }

  /**
   * Exposes an index validity check through the polyglot API.
   *
   * @param index the index to check
   * @return {@code true} if the index is valid, {@code false} otherwise.
   */
  @ExportMessage
  boolean isArrayElementReadable(long index) {
    try {
      return index < getArraySize() && index >= 0;
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
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().vector();
  }
}
