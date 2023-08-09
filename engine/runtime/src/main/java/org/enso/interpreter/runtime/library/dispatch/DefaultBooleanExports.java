package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;

@ExportLibrary(value = TypesLibrary.class, receiverType = Boolean.class)
public class DefaultBooleanExports {
  @ExportMessage
  static boolean hasType(Boolean receiver) {
    return true;
  }

  @ExportMessage
  static Type getType(
      Boolean receiver,
      @CachedLibrary("receiver") TypesLibrary thisLib,
      @Cached(value = "1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().bool().getType();
  }
}
