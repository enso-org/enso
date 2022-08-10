package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Type;

@ExportLibrary(value = TypesLibrary.class, receiverType = Double.class)
public class DefaultDoubleExports {
  @ExportMessage
  static boolean hasType(Double receiver) {
    return true;
  }

  @ExportMessage
  static Type getType(Double receiver, @CachedLibrary("receiver") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().number().getDecimal();
  }
}
