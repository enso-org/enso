package org.enso.base_test_helpers;

import java.util.function.Function;
import org.graalvm.polyglot.Value;

public class CallbackHelper {
  public static Value runCallbackInt(Function<Integer, Value> callback, int x) {
    return callback.apply(x);
  }

  public static Value rethrow(Value errorType, long value) {
    var error = errorType.invokeMember("throw", value);
    throw error.throwException();
  }

  public static Value error(Value errorType, long value) {
    var error = errorType.invokeMember("throw", value);
    return error;
  }
}
