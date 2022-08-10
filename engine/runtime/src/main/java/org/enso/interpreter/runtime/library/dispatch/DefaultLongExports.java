package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Type;

@ExportLibrary(value = TypesLibrary.class, receiverType = Long.class)
public class DefaultLongExports {
  @ExportMessage
  static boolean hasType(Long receiver) {
    return true;
  }

  @ExportMessage
  static Type getType(Long receiver, @CachedLibrary("receiver") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().number().getSmallInteger();
  }
}
