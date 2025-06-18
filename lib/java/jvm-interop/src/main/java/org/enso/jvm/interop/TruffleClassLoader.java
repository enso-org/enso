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
  private Context ctx;
  private Object value;

  TruffleClassLoader() {
    super(new URL[0]);
  }

  final synchronized void assignCtx(Context ctx) {
    assert this.ctx == null;
    this.ctx = ctx;
  }

  private synchronized Context ctx() {
    if (ctx == null) {
      ctx =
          Context.newBuilder() // no dynamic languages needed
              .allowHostAccess(HostAccess.ALL) // all public members
              .allowExperimentalOptions(true) // to survive any -Dpolyglot options
              .build();
    }
    return ctx;
  }

  final <D> D withCtx(Supplier<D> action) {
    ctx().enter();
    try {
      return action.get();
    } finally {
      ctx().leave();
    }
  }

  void addToClassPath(String url) {
    try {
      addURL(new URI(url).toURL());
    } catch (MalformedURLException | URISyntaxException ex) {
      ex.printStackTrace();
    }
  }

  final TruffleObject loadClassObject(String className) throws ClassNotFoundException {
    var clazz = loadClass(className);
    var clazzValue1 = ctx().asValue(clazz);
    var clazzValue2 = clazzValue1.getMember("static");
    ctx().asValue(this).execute(clazzValue2);
    return (TruffleObject) value;
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
