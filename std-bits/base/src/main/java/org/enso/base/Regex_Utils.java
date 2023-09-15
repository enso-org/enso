package org.enso.base;

public class Regex_Utils {

  /**
   * Converts a SQL-like pattern into a Regex with the same semantics.
   *
   * <p>Special regex characters present in the input pattern are quoted to match them literally
   * according to the SQL-like format.
   */
  public static String sql_like_pattern_to_regex(String sql_pattern) {
    StringBuilder result = new StringBuilder();
    // Accumulates the intermittent characters between wildcards. These will be quoted in bulk.
    StringBuilder acc = new StringBuilder();
    for (int i = 0; i < sql_pattern.length(); ++i) {
      char c = sql_pattern.charAt(i);
      if (c == '%' || c == '_') {
        // Before inserting the converted wildcard, we append the accumulated characters, quoting
        // them first.
        if (acc.length() > 0) {
          result.append(regexQuote(acc.toString()));
          acc.setLength(0);
        }

        if (c == '%') {
          result.append(".*");
        } else {
          result.append(".");
        }
      } else {
        acc.append(c);
      }
    }

    // If any trailing characters were left, we append them too.
    if (acc.length() > 0) {
      result.append(regexQuote(acc.toString()));
    }

    return result.toString();
  }

  public static String regexQuote(String pattern) {
    return pattern.replaceAll("[.*+?^${}()|\\[\\]]", "\\\\$0");
  }
}
