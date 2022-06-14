package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.InvalidArrayIndexError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

import java.util.Arrays;

/** A primitive boxed array type for use in the runtime. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Data.Array.Array")
public class Array implements TruffleObject {
  private final Object[] items;

  /**
   * Creates a new array
   *
   * @param items the element values
   */
  @Builtin.Method(expandVarargs = 4, description = "Creates an array with given elements.")
  public Array(Object... items) {
    this.items = items;
  }

  /**
   * Creates an uninitialized array of the given size.
   *
   * @param size the size of the created array.
   */
  @Builtin.Method(description = "Creates an uninitialized array of a given size.")
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

  /** @return the size of this array */
  @Builtin.Method(description = "Returns the size of this array.")
  public long length() {
    return this.getItems().length;
  }

  /** @return an empty array */
  @Builtin.Method(description = "Creates an empty Array")
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

  @Builtin.Method(name = "at", description = "Gets an array element at the given index.")
  @Builtin.WrapException(from = IndexOutOfBoundsException.class, to = InvalidArrayIndexError.class)
  public Object get(long index) {
    return getItems()[(int) index];
  }

  @Builtin.Method(name = "setAt", description = "Gets an array element at the given index.")
  @Builtin.WrapException(from = IndexOutOfBoundsException.class, to = InvalidArrayIndexError.class)
  public Object set(long index, @AcceptsError Object value) {
    getItems()[(int) index] = value;
    return this;
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

  @Override
  public String toString() {
    return Arrays.toString(items);
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(UnresolvedSymbol symbol) {
      Context context = getContext();
      return symbol.resolveFor(context.getBuiltins().array(), context.getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Array _this,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Array _this, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  static boolean canConvertFrom(Array receiver) {
    return true;
  }

  @ExportMessage
  static class GetConversionFunction {
    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(AtomConstructor target, UnresolvedConversion conversion) {
      Context context = getContext();
      return conversion.resolveFor(
          target, context.getBuiltins().array(), context.getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Array _this,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("doResolve(cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Array _this, AtomConstructor target, UnresolvedConversion conversion)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
