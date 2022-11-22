package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
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
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * A wrapper that allows to turn an Enso callback providing elements into a polyglot Array.
 *
 * <p>This allows creation of arrays (and with them, vectors) using non-standard storage - for
 * example exposing rows of a Table without copying any data.
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Data.Array_Proxy.Array_Proxy")
public final class ArrayProxy implements TruffleObject {
  private final long length;
  private final Object at;

  @Builtin.Method(description = "Creates an array backed by a proxy object.")
  public ArrayProxy(long length, Object at) {
    if (CompilerDirectives.inInterpreter()) {
      InteropLibrary interop = InteropLibrary.getUncached();
      if (!interop.isExecutable(at)) {
        throw new PanicException(
            Context.get(interop).getBuiltins().error().makeTypeError("Function", at, "at"),
            interop);
      }
    }

    this.length = length;
    this.at = at;
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
  public Object readArrayElement(long index, @CachedLibrary(limit = "3") InteropLibrary interop)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    if (index >= length || index < 0) {
      throw InvalidArrayIndexException.create(index);
    }

    try {
      return interop.execute(at, index);
    } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException e) {
      throw UnsupportedMessageException.create(e);
    }
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
