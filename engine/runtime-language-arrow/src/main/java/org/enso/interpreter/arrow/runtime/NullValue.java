package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(InteropLibrary.class)
final class NullValue implements TruffleObject {

  @CompilerDirectives.CompilationFinal private static NullValue value = null;

  private NullValue() {}

  public static NullValue get() {
    if (value == null) {
      value = new NullValue();
    }
    return value;
  }

  @ExportMessage
  public boolean isNull() {
    return true;
  }
}
