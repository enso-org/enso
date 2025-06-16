package org.enso.jvm.interop;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.function.Supplier;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;

@ExportLibrary(value = InteropLibrary.class)
final class TruffleClassLoader extends URLClassLoader implements TruffleObject {
  private static final TruffleClassLoader DEFAULT = new TruffleClassLoader();
  private static Context ctx;
  private Object value;

  private TruffleClassLoader() {
    super(new URL[0]);
  }

  static <D> D withCtx(Supplier<D> action) {
    ctx().enter();
    try {
      return action.get();
    } finally {
      ctx().leave();
    }
  }

  private static synchronized Context ctx() {
    if (ctx == null) {
      ctx =
          Context.newBuilder() // no dynamic languages needed
              .allowHostAccess(HostAccess.ALL) // all public members
              .allowExperimentalOptions(true) // to survive any -Dpolyglot options
              .build();
    }
    return ctx;
  }

  static void addToClassPath(String url) {
    try {
      DEFAULT.addURL(new URI(url).toURL());
    } catch (MalformedURLException | URISyntaxException ex) {
      ex.printStackTrace();
    }
  }

  static TruffleObject loadClassObject(String className) throws ClassNotFoundException {
    var clazz = DEFAULT.loadClass(className);
    var clazzValue1 = ctx().asValue(clazz);
    var clazzValue2 = clazzValue1.getMember("static");
    ctx().asValue(DEFAULT).execute(clazzValue2);
    return (TruffleObject) DEFAULT.value;
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
