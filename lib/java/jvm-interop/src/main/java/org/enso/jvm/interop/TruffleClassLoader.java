package org.enso.jvm.interop;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.graalvm.polyglot.Value;

@ExportLibrary(value = InteropLibrary.class)
final class TruffleClassLoader implements TruffleObject {

  private Object value;

  private TruffleClassLoader() {}

  static TruffleObject loadClass(String className) throws ClassNotFoundException {
    java.lang.Class<?> clazz = Class.forName(className);
    org.graalvm.polyglot.Value clazzValue = Value.asValue(clazz).getMember("static");
    org.enso.jvm.interop.TruffleClassLoader holderRaw = new TruffleClassLoader();
    org.graalvm.polyglot.Value holderValue = Value.asValue(holderRaw);
    holderValue.putMember("any", clazzValue);
    java.lang.Object clazzRaw = holderRaw.value;
    return (TruffleObject) clazzRaw;
  }

  @ExportMessage
  void writeMember(String name, Object value) {
    this.value = value;
  }

  @ExportMessage
  boolean hasMembers() {
    return false;
  }

  @ExportMessage
  boolean isMemberModifiable(String member) {
    return true;
  }

  @ExportMessage
  boolean isMemberInsertable(String member) {
    return false;
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) {
    return this;
  }
}
