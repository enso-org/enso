package org.enso.polyglot.common_utils;

import com.ibm.icu.text.BreakIterator;

public class Core_Text_Utils {
  /** Computes the length of the string as the number of grapheme clusters it contains. */
  public static int computeGraphemeLength(String text) {
    BreakIterator iter = BreakIterator.getCharacterInstance();
    iter.setText(text);
    int len = 0;
    while (iter.next() != BreakIterator.DONE) {
      len++;
    }
    return len;
  }

  /** Pretty prints the string, escaping special characters. */
  public static String prettyPrint(String str) {
    int len = str.length();
    int outputLength = len + 2; // Precise if there are no special characters.

    // TODO This should be more extensible; while it's still a small fixed set,
    // a switch is probably fastest (unconfirmed)

    StringBuilder sb = new StringBuilder(outputLength);

    sb.append('\'');

    for (int i = 0; i < len; ++i) {
      char c = str.charAt(i);
      switch (c) {
        case '\\' -> sb.append("\\\\");
        case '\'' -> sb.append("\\'");
        case '\n' -> sb.append("\\n");
        case '\t' -> sb.append("\\t");
        case '\0' -> sb.append("\\0");
        case '\u0007' -> sb.append("\\a");
        case '\u0008' -> sb.append("\\b");
        case '\u000c' -> sb.append("\\f");
        case '\r' -> sb.append("\\r");
        case '\u000B' -> sb.append("\\v");
        case '\u001B' -> sb.append("\\e");
        default -> sb.append(c);
      }
    }

    sb.append('\'');

    return sb.toString();
  }
}
