package org.enso.base;

import java.util.Map;
import java.util.regex.Pattern;

public class Regex_Utils {

  /**
   * Obtains the names for named groups.
   *
   * @param pattern the pattern for which to get the group names
   * @return the names for the named groups in {@code pattern}
   */
  public static String[] get_group_names(Pattern pattern) {
    return get_named_groups_mapping(pattern).values().toArray(new String[0]);
  }

  /**
   *
   * This must only be called on regexes that have successfully been parsed.
   *
   * @param pattern
   * @return
   */
  public static Map<Integer, String> get_named_groups_mapping(Pattern pattern) {
    String regex = pattern.pattern();

    // TODO Shotgun parser that matches (X) and (?<name>X) (but not the others)

    return null;
  }
}
