package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import org.graalvm.polyglot.Value;

@ExportLibrary(value = InteropLibrary.class)
final class TruffleClassLoader extends URLClassLoader implements TruffleObject {
  private Object value;

  TruffleClassLoader() {
    super(new URL[0]);
  }

  void addToClassPath(String file) {
    try {
      addURL(new File(file).toURI().toURL());
    } catch (MalformedURLException ex) {
      ex.printStackTrace();
    }
  }

  final TruffleObject loadClassObject(String className) throws ClassNotFoundException {
    var clazz = loadClass(className);
    var clazzValue = Value.asValue(clazz);
    var clazzStatics = clazzValue.getMember("static");
    return extractRawValue(clazzStatics);
  }

  private synchronized TruffleObject extractRawValue(Value v) {
    Value.asValue(this).execute(v);
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
