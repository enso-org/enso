package org.enso.table.util;

public class MatchHelper {
  public static String[] matchExact(String[] presentColumnNames, String[] requestedNames, boolean caseSensitive) {
    if (!caseSensitive) return matchCaseInsensitive(presentColumnNames, requestedNames);

    // TODO
    throw new RuntimeException("Not implemented.");
  }

  public static String[] matchCaseInsensitive(String[] presentColumnNames, String[] requestedNames) {
    // TODO
    throw new RuntimeException("Not implemented.");
  }

  public static String[] matchRegex(String[] presentColumnNames, String[] regexes, boolean caseSensitive) {
    // TODO
    throw new RuntimeException("Not implemented.");
  }
}
