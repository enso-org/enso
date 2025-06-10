package org.enso.jvm.interop;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;

@ExportLibrary(value = InteropLibrary.class)
final class TruffleClassLoader implements TruffleObject {
  private static Context ctx;

  private Object value;

  private TruffleClassLoader() {}

  static synchronized Context ctx() {
    if (ctx == null) {
      ctx =
          Context.newBuilder() // no dynamic languages needed
              .allowHostAccess(HostAccess.ALL) // all public members
              .build();
    }
    return ctx;
  }

  static TruffleObject loadClass(String className) throws ClassNotFoundException {
    var context = ctx();

    var clazz = Class.forName(className);
    var clazzValue1 = context.asValue(clazz);
    var clazzValue2 = clazzValue1.getMember("static");
    var holderRaw = new TruffleClassLoader();
    var holderValue = context.asValue(holderRaw);
    holderValue.execute(clazzValue2);
    var clazzRaw = holderRaw.value;
    return (TruffleObject) clazzRaw;
  }

  @ExportMessage
  final Object execute(Object[] values) {
    this.value = values[0];
    return this;
  }

  @ExportMessage
  final boolean isExecutable() {
    return true;
  }
}
