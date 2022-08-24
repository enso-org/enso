package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Data.Vector.Vector")
public class Vector implements TruffleObject {
  private final Object storage;

  public Vector(Object storage) {
    this.storage = storage;
  }

  @Builtin.Method(name = "new_builtin", description = "Returns the length of this Vector.")
  @Builtin.Specialize
  public static Vector newFromFunction(long length, Function fun, InteropLibrary interop) {
    Object[] target = new Object[(int) length];
    for (int i = 0; i < length; i++) {
      try {
        target[i] = interop.execute(fun, (long) i);
      } catch (ArityException e) {
        e.printStackTrace(); // todo propagate
      } catch (UnsupportedTypeException e) {
        e.printStackTrace(); // todo propagate
      } catch (UnsupportedMessageException e) {
        e.printStackTrace(); // todo propagate
      }
    }
    return new Vector(new Array(target));
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
  @Builtin.WrapException(
      from = UnsupportedMessageException.class,
      to = PolyglotError.class,
      propagate = true)
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
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(UnresolvedSymbol symbol) {
      Context context = getContext();
      return symbol.resolveFor(context.getBuiltins().vector(), context.getBuiltins().any());
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
        Vector self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Vector self, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  static boolean canConvertFrom(Vector receiver) {
    return true;
  }

  @ExportMessage
  static class GetConversionFunction {
    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(AtomConstructor target, UnresolvedConversion conversion) {
      Context context = getContext();
      return conversion.resolveFor(
          target, context.getBuiltins().vector(), context.getBuiltins().any());
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
        Vector self,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("doResolve(cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Vector self, AtomConstructor target, UnresolvedConversion conversion)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
