package org.enso.runtime.utils;

import java.util.Arrays;

public class ThreadUtils {

  private ThreadUtils() {}

  public static String dumpAllStacktraces(String header) {
    return dumpAllStacktraces("", header);
  }

  public static String dumpAllStacktraces(String prefix, String header) {
    var sb = new StringBuilder();
    sb.append(header);
    sb.append(System.lineSeparator());
    Thread.getAllStackTraces()
        .entrySet()
        .forEach(
            entry -> {
              sb.append(prefix).append(entry.getKey().getName()).append(System.lineSeparator());
              Arrays.stream(entry.getValue())
                  .forEach(
                      e ->
                          sb.append(prefix)
                              .append("    at ")
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
