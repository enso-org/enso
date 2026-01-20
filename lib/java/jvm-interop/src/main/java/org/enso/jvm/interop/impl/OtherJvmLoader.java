package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;

/** Handles classloading in the "slave" JVM. */
@ExportLibrary(value = InteropLibrary.class)
final class OtherJvmLoader extends URLClassLoader implements TruffleObject {
  final Context ctx;
  private TruffleObject findLibraries;
  private Object value;

  OtherJvmLoader() {
    super(new URL[0]);
    ctx =
        Context.newBuilder("host") // no dynamic languages needed
            .allowHostAccess(HostAccess.ALL) // all public members
            .allowExperimentalOptions(true) // to survive any -Dpolyglot options
            .build();
  }

  final void addToClassPath(String file) {
    try {
      addURL(new File(file).toURI().toURL());
    } catch (MalformedURLException ex) {
      ex.printStackTrace();
    }
  }

  final void findLibraries(TruffleObject obj) {
    this.findLibraries = obj;
  }

  @Override
  protected String findLibrary(String libName) {
    if (this.findLibraries != null) {
      try {
        var iop = InteropLibrary.getUncached();
        var mayBePath = iop.execute(this.findLibraries, libName);
        if (iop.isString(mayBePath)) {
          return iop.asString(mayBePath);
        }
      } catch (InteropException ex) {
        var logger = System.getLogger("org.enso.jvm.interop");
        logger.log(System.Logger.Level.WARNING, ex);
      }
    }
    return null;
  }

  final TruffleObject loadClassObject(String className) throws ClassNotFoundException {
    var clazz = loadClass(className);
    var clazzValue1 = ctx.asValue(clazz);
    var clazzValue2 = clazzValue1.getMember("static");
    ctx.asValue(this).execute(clazzValue2);
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
