package org.enso.compiler.dump.igv;

final class Utils {
  private Utils() {}

  static String label(Object obj) {
    var className = strippedClassName(obj.getClass().getName());
    var hash = Integer.toHexString(System.identityHashCode(obj));
    return className + "_" + hash;
  }

  private static String strippedClassName(String fqn) {
    return fqn.replace("org.enso.compiler.core.ir.", "");
  }
}
