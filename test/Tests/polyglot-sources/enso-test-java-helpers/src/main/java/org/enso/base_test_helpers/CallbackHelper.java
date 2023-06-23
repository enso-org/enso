package org.enso.base_test_helpers;

import java.util.function.Function;
import org.graalvm.polyglot.Value;

public class CallbackHelper {
  public static Value runCallbackInt(Function<Integer, Value> callback, int x) {
    return callback.apply(x);
  }
}
