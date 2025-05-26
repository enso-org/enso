package org.enso.runtime.utils;

import java.util.Arrays;

public class ThreadUtils {

  private ThreadUtils() {}

  public static String dumpAllStacktraces(String prefix) {
    var sb = new StringBuilder(prefix);
    sb.append(System.lineSeparator());
    Thread.getAllStackTraces()
        .entrySet()
        .forEach(
            entry -> {
              sb.append(entry.getKey().getName()).append(System.lineSeparator());
              Arrays.stream(entry.getValue())
                  .forEach(
                      e ->
                          sb.append("    ")
                              .append(e.getClassName())
                              .append(".")
                              .append(e.getMethodName())
                              .append("(")
                              .append(e.getFileName())
                              .append(":")
                              .append(e.getLineNumber())
                              .append(")")
                              .append(System.lineSeparator()));
            });
    return sb.toString();
  }
}
