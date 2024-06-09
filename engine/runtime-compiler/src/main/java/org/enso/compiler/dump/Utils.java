package org.enso.compiler.dump;

final class Utils {
  private Utils() {}

  static String id(Object obj) {
    var className = obj.getClass().getSimpleName();
    var hash = Integer.toHexString(System.identityHashCode(obj));
    return className + "_" + hash;
  }

  static boolean hasOneLine(String label) {
    return label.lines().count() == 1;
  }

  static boolean isSurroundedByQuotes(String str) {
    return str.startsWith("\"") && str.endsWith("\"");
  }
}
