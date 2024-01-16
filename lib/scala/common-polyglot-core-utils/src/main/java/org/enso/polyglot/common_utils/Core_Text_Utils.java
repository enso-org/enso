package org.enso.polyglot.common_utils;

import com.ibm.icu.text.BreakIterator;
import com.ibm.icu.text.Normalizer;
import com.ibm.icu.text.Normalizer2;

public class Core_Text_Utils {
  private Core_Text_Utils() {}

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

  /** Computes a hashcode of a string that is insensitive to Unicode normalization. */
  public static int unicodeNormalizedHashCode(String str) {
    Normalizer2 normalizer = Normalizer2.getNFDInstance();
    return normalizer.normalize(str).hashCode();
  }

  /**
   * Checks whether two strings are equal up to Unicode canonicalization.
   *
   * @param str1 the first string
   * @param str2 the second string
   * @return the result of comparison
   */
  public static boolean equals(String str1, String str2) {
    return compare_normalized(str1, str2) == 0;
  }

  /**
   * Compares {@code a} to {@code b} according to the lexicographical order, handling Unicode
   * normalization.
   *
   * @param a the left operand
   * @param b the right operand
   * @return a negative value if {@code a} is before {@code b}, 0 if both values are equal and a
   *     positive value if {@code a} is after {@code b}
   */
  public static int compare_normalized(String a, String b) {
    return Normalizer.compare(a, b, Normalizer.FOLD_CASE_DEFAULT);
  }

  /** Returns a prefix of the string not exceeding the provided grapheme length. */
  public static String take_prefix(String str, long grapheme_length) {
    BreakIterator iter = BreakIterator.getCharacterInstance();
    iter.setText(str);
    if (grapheme_length <= 0) {
      return "";
    } else if (iter.next(Math.toIntExact(grapheme_length)) == BreakIterator.DONE) {
      return str;
    } else {
      return str.substring(0, iter.current());
    }
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
