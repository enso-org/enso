package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(InteropLibrary.class)
public class CurriedMethod implements TruffleObject {
  final Function function;
  private final Object self;

  public CurriedMethod(Function function, Object self) {
    this.function = function;
    this.self = self;
  }

  @ExportMessage
  public boolean isExecutable() {
    return true;
  }

  @ExportMessage
  public Object execute(
      Object[] arguments, @CachedLibrary("this.function") InteropLibrary functions)
      throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
    Object[] args = new Object[arguments.length + 1];
    args[0] = this.self;
    System.arraycopy(arguments, 0, args, 1, arguments.length);
    return functions.execute(this.function, args);
  }
}
