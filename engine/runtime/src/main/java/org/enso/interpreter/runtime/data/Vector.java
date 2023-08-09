package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.frame.VirtualFrame;
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
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

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
  @Builtin.Specialize()
  public static Object newFromFunction(
      VirtualFrame frame,
      long length,
      Function fun,
      State state,
      @Cached("buildWithArity(1)") InvokeFunctionNode invokeFunctionNode) {
    Object[] target = new Object[Math.toIntExact(length)];
    for (int i = 0; i < target.length; i++) {
      var value = invokeFunctionNode.execute(fun, frame, state, new Long[] {(long) i});
      if (value instanceof DataflowError) {
        return value;
      }
      target[i] = value;
    }
    return new Vector(new Array(target));
  }

  @Builtin.Method(
      name = "to_array",
      description = "Returns an Array representation of this Vector.")
  public final Object toArray() {
    return this.storage;
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
  long getArraySize(@Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop)
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
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop,
      @CachedLibrary(limit = "3") WarningsLibrary warnings,
      @Cached HostValueToEnsoNode toEnso)
      throws InvalidArrayIndexException, UnsupportedMessageException {
    var v = interop.readArrayElement(this.storage, index);
    if (warnings.hasWarnings(this.storage)) {
      Warning[] extracted = warnings.getWarnings(this.storage, null);
      if (warnings.hasWarnings(v)) {
        v = warnings.removeWarnings(v);
      }
      return WithWarnings.wrap(EnsoContext.get(interop), toEnso.execute(v), extracted);
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
  boolean isArrayElementReadable(
      long index, @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
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
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().vector();
  }

  @ExportMessage
  boolean hasWarnings(@Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    return warnings.hasWarnings(this.storage);
  }

  @ExportMessage
  Warning[] getWarnings(
      Node location, @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    return warnings.getWarnings(this.storage, location);
  }

  @ExportMessage
  Vector removeWarnings(@Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    return new Vector(warnings.removeWarnings(this.storage));
  }

  @ExportMessage
  boolean isLimitReached(@Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    return warnings.isLimitReached(this.storage);
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
