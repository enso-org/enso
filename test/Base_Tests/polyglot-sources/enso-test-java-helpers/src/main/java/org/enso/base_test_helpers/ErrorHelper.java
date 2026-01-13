package org.enso.base_test_helpers;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public final class ErrorHelper {
  private ErrorHelper() {}

  public static void throwError(String msg) {
    var error = returnError(msg);
    throw error.throwException();
  }

  public static Value returnError(String msg) {
    var type = EnsoMeta.getType("Standard.Base.Errors.Common", "Compile_Error");
    var cons = type.invokeMember("Error");
    var atom = cons.newInstance(msg);
    var errorType = EnsoMeta.getType("Standard.Base.Error", "Error");
    var error = errorType.invokeMember("throw", atom);
    return error;
  }
}
