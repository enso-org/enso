package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Data.Array_Proxy.Array_Proxy")
public class ArrayProxy implements TruffleObject {
  private final long length;
  private final Object at;

  @Builtin.Method(description = "Creates an array backed by a proxy object.")
  public ArrayProxy(long length, Object at) {
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
}
