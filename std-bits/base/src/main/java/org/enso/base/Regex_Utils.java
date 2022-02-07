package org.enso.base;

import java.util.ArrayList;
import java.util.regex.Pattern;

public class Regex_Utils {

  /**
   * Obtains the names for named groups.
   *
   * <p>Assumes that the provided {@link Pattern} is syntactically valid. Behaviour is undefined if
   * run on a syntactically invalid pattern.
   *
   * @param pattern the pattern for which to get the group names
   * @return the names for the named groups in {@code pattern}
   */
  public static String[] get_group_names(Pattern pattern) {
    String pattern_text = pattern.pattern();

    char[] characters = pattern_text.toCharArray();
    ArrayList<String> names = new ArrayList<>();

    for (int i = 0; i < pattern_text.length(); ++i) {
      char character = characters[i];

      if (character == '\\') {
        ++i;
        break;
      }

      String header = "(?<";

      if (pattern_text.startsWith(header, i)) {
        i += header.length();
        StringBuilder buffer = new StringBuilder();

        while (i < pattern_text.length()) {
          character = characters[i];

          if (character == '>') {
            break;
          }

          ++i;

          buffer.append(character);
        }

        names.add(buffer.toString());
      }
    }

    return names.toArray(new String[0]);
  }
}
