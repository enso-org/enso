package org.enso.base_test_helpers;

import org.graalvm.polyglot.Value;

import java.util.function.Function;

public class CallbackHelper {
  public static Value runCallbackInt(Function<Integer, Value> callback, int x) {
    return callback.apply(x);
  }
}
