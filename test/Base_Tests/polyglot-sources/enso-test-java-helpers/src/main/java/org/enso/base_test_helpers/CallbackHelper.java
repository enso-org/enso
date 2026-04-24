package org.enso.base_test_helpers;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.ArrayList;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
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

  /**
   * Intricate way to create {@link ArrayList} that is capable to keep {@code WithWarnings} wrappers
   * in the newly created list.
   *
   * @param size
   * @param provider
   * @return
   */
  public static ArrayList<Object> createArrayList(int size, Function<Integer, Value> provider) {
    var arr = new ArrayList<Object>();
    for (var i = 0; i < size; i++) {
      var v = provider.apply(i);
      var unwrap = new Unwrap();
      var unwrapValue = Context.getCurrent().asValue(unwrap);
      unwrapValue.execute(v);
      arr.add(unwrap.arg);
    }
    return arr;
  }

  @ExportLibrary(InteropLibrary.class)
  static final class Unwrap implements TruffleObject {
    Object arg;

    @ExportMessage
    public Object execute(Object[] args) {
      return arg = args[0];
    }

    @ExportMessage
    boolean isExecutable() {
      return true;
    }
  }
}
