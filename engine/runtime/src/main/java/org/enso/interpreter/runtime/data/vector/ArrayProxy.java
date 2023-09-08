package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * A wrapper that allows to turn an Enso callback providing elements into a polyglot Array.
 *
 * <p>This allows creation of arrays (and with them, vectors) using non-standard storage - for
 * example exposing rows of a Table without copying any data.
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@ImportStatic(BranchProfile.class)
final class ArrayProxy implements EnsoObject {
  private final long length;
  private final Object at;

  private ArrayProxy(long length, Object at) throws IllegalArgumentException {
    if (CompilerDirectives.inInterpreter()) {
      InteropLibrary interop = InteropLibrary.getUncached();
      if (!interop.isExecutable(at)) {
        var msg = "Array_Proxy needs executable function.";
        throw ArrayPanics.typeError(interop, at, msg);
      }
    }

    if (length < 0) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalArgumentException("Array_Proxy length cannot be negative.");
    }

    this.length = length;
    this.at = at;
  }

  static ArrayProxy create(long length, Object at) throws IllegalArgumentException {
    return new ArrayProxy(length, at);
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  public long getArraySize() {
    return length;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index < length && index >= 0;
  }

  @ExportMessage
  public Object readArrayElement(
      long index,
      @Cached("create()") BranchProfile arrayIndexHasHappened,
      @CachedLibrary(limit = "3") InteropLibrary interop)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    try {
      if (index >= length || index < 0) {
        arrayIndexHasHappened.enter();
        throw InvalidArrayIndexException.create(index);
      }
      return interop.execute(at, index);
    } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException e) {
      throw UnsupportedMessageException.create(e);
    }
  }

  @ExportMessage
  String toDisplayString(boolean b) {
    return toString();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().array();
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return "(Array_Proxy " + length + " " + at + ")";
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().array();
  }
}
