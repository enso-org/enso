package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(
    pkg = "mutable",
    stdlibName = "Standard.Base.Data.Polyglot_Array_Builder.Polyglot_Array_Builder")
public abstract class PolyglotArrayBuilder implements EnsoObject {

  protected boolean sealed = false;

  public void seal() {
    sealed = true;
  }

  public abstract void append(Object element, InteropLibrary interop)
      throws InvalidArrayIndexException, UnsupportedMessageException, UnsupportedTypeException;

  public abstract int getCurrentIndex();

  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    throw CompilerDirectives.shouldNotReachHere();
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    throw CompilerDirectives.shouldNotReachHere();
  }

  @ExportMessage
  long getArraySize() {
    throw CompilerDirectives.shouldNotReachHere();
  }

  @ExportMessage
  Object readArrayElement(long index)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    throw CompilerDirectives.shouldNotReachHere();
  }

  @ExportMessage
  void writeArrayElement(long index, Object value) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    throw CompilerDirectives.shouldNotReachHere();
  }

  @ExportMessage
  boolean isArrayElementRemovable(long index) {
    throw CompilerDirectives.shouldNotReachHere();
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
    return EnsoContext.get(thisLib).getBuiltins().polyglotArrayBuilder();
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
    return EnsoContext.get(thisLib).getBuiltins().polyglotArrayBuilder();
  }

  //
  // helper methods
  //

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return toDisplayString(false);
  }

  @ExportLibrary(InteropLibrary.class)
  @ExportLibrary(WarningsLibrary.class)
  static final class GenericArrowArray extends PolyglotArrayBuilder {
    private final Object storage;
    private int index;

    private GenericArrowArray(Object storage) {
      if (CompilerDirectives.inInterpreter()) {
        if (!InteropLibrary.getUncached().hasArrayElements(storage)) {
          throw EnsoContext.get(null)
              .raiseAssertionPanic(
                  null,
                  "Polyglot_Array_Builder needs array-like delegate, but got: " + storage,
                  null);
        }
      }
      this.storage = storage;
      this.index = 0;
    }

    final Object toArray() {
      return this.storage;
    }

    //
    // messages for the InteropLibrary
    //

    @ExportMessage
    long getArraySize(
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop)
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
    Object readArrayElement(
        long index,
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop,
        @CachedLibrary(limit = "3") WarningsLibrary warnings,
        @Cached HostValueToEnsoNode toEnso)
        throws InvalidArrayIndexException, UnsupportedMessageException {
      var v = interop.readArrayElement(this.storage, index);
      if (warnings.hasWarnings(this.storage)) {
        Warning[] extracted = warnings.getWarnings(this.storage, null, false);
        if (warnings.hasWarnings(v)) {
          v = warnings.removeWarnings(v);
        }
        return WithWarnings.wrap(EnsoContext.get(interop), toEnso.execute(v), extracted);
      }
      return toEnso.execute(v);
    }

    @ExportMessage
    void writeArrayElement(
        long index,
        Object element,
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop)
        throws InvalidArrayIndexException, UnsupportedMessageException, UnsupportedTypeException {
      if (!interop.isArrayElementWritable(this.storage, index) || sealed) {
        throw UnsupportedMessageException.create();
      }
      interop.writeArrayElement(this.storage, index, element);
    }

    /**
     * Exposes an index validity check through the polyglot API.
     *
     * @param index the index to check
     * @return {@code true} if the index is valid, {@code false} otherwise.
     */
    @ExportMessage
    boolean isArrayElementReadable(
        long index,
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
      return interop.isArrayElementReadable(this.storage, index);
    }

    @ExportMessage
    boolean isArrayElementModifiable(
        long index,
        @Cached.Shared(value = "interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
      return interop.isArrayElementModifiable(this.storage, index);
    }

    @ExportMessage
    boolean hasWarnings(
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
      return warnings.hasWarnings(this.storage);
    }

    @ExportMessage
    Warning[] getWarnings(
        Node location,
        boolean shouldWrap,
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
        throws UnsupportedMessageException {
      return warnings.getWarnings(this.storage, location, shouldWrap);
    }

    @ExportMessage
    GenericArrowArray removeWarnings(
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
        throws UnsupportedMessageException {
      return new GenericArrowArray(warnings.removeWarnings(this.storage));
    }

    @ExportMessage
    boolean isLimitReached(
        @Cached.Shared(value = "warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings) {
      return warnings.isLimitReached(this.storage);
    }

    @Override
    public void append(Object element, InteropLibrary interop)
        throws InvalidArrayIndexException, UnsupportedMessageException, UnsupportedTypeException {
      var current = index;
      this.writeArrayElement(current, element, interop);
      this.index = current + 1;
    }

    @Override
    public int getCurrentIndex() {
      return index;
    }
  }

  public static EnsoObject fromArray(Object arrowArray) {
    return new PolyglotArrayBuilder.GenericArrowArray(arrowArray);
  }
}
