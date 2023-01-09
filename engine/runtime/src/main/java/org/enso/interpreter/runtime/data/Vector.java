package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.*;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Data.Vector.Vector")
public final class Vector implements TruffleObject {
  private final Object storage;

  private Vector(Object storage) {
    if (CompilerDirectives.inInterpreter()) {
      if (!InteropLibrary.getUncached().hasArrayElements(storage)) {
        throw new IllegalStateException("Vector needs array-like delegate, but got: " + storage);
      }
    }
    this.storage = storage;
  }

  @Builtin.Method(
      name = "new",
      description = "Creates new Vector with given length and provided elements.",
      autoRegister = false)
  @Builtin.Specialize
  public static Object newFromFunction(long length, Function fun, InteropLibrary interop) {
    Object[] target = new Object[Math.toIntExact(length)];
    for (int i = 0; i < target.length; i++) {
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

  @Builtin.Method(
      name = "to_array",
      description = "Returns an Array representation of this Vector.")
  public final Object toArray() {
    return this.storage;
  }

  @Builtin.Method(name = "slice", description = "Returns a slice of this Vector.")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  public final Vector slice(long start, long end, InteropLibrary interop)
      throws UnsupportedMessageException {
    long this_length = length(interop);
    long slice_start = Math.max(0, start);
    long slice_end = Math.min(this_length, end);

    if (slice_start >= slice_end) {
      return new Vector(new Array(0));
    }

    if ((slice_start == 0) && (slice_end == this_length)) {
      return this;
    }

    return new Vector(new ArraySlice(this.storage, slice_start, slice_end));
  }

  @Builtin.Method(description = "Returns the length of this Vector.")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  public final long length(InteropLibrary interop) throws UnsupportedMessageException {
    return interop.getArraySize(storage);
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
  long getArraySize(@CachedLibrary(limit = "3") InteropLibrary interop)
      throws UnsupportedMessageException {
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
  public Object readArrayElement(
      long index,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @Cached HostValueToEnsoNode toEnso)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    var v = interop.readArrayElement(storage, index);
    if (warnings.hasWarnings(this)) {
      Warning[] extracted = warnings.getWarnings(this, null);
      if (warnings.hasWarnings(v)) {
        v = warnings.removeWarnings(v);
      }
      return new WithWarnings(toEnso.execute(v), extracted);
    }
    return toEnso.execute(v);
  }

  public static Vector fromArray(Object arr) {
    return new Vector(arr);
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
      var size = interop.getArraySize(storage);
      return index < size && index >= 0;
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
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().vector();
  }

  @ExportMessage
  boolean hasWarnings(@CachedLibrary(limit = "3") WarningsLibrary warnings) {
    return warnings.hasWarnings(this.storage);
  }

  @ExportMessage
  Warning[] getWarnings(Node location, @CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    return warnings.getWarnings(this.storage, location);
  }

  @ExportMessage
  Vector removeWarnings(@CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    return new Vector(warnings.removeWarnings(this.storage));
  }

  //
  // helper methods
  //

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return toDisplayString(false);
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
