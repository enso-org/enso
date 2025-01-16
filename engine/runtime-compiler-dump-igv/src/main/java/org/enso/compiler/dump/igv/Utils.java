package org.enso.compiler.dump.igv;

final class Utils {
  private Utils() {}

  static String label(Object obj) {
    var className = obj.getClass().getSimpleName();
    var hash = Integer.toHexString(System.identityHashCode(obj));
    return className + "_" + hash;
  }
}
