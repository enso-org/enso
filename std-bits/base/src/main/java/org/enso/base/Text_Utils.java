package org.enso.base;

import com.ibm.icu.lang.UCharacter;
import com.ibm.icu.text.Normalizer;
import com.ibm.icu.text.Normalizer2;
import com.ibm.icu.text.StringSearch;
import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/** Utils for standard library operations on Text. */
public class Text_Utils {
  private static final Pattern whitespace =
      Pattern.compile("\\s+", Pattern.UNICODE_CHARACTER_CLASS);
  private static final Pattern vertical_space =
      Pattern.compile("\\v+", Pattern.UNICODE_CHARACTER_CLASS);

  /**
   * Creates a substring of the given string, indexing using the Java standard (UTF-16) indexing
   * mechanism.
   *
   * @param string the string to substring
   * @param from starting index
   * @param to index one past the end of the desired substring
   * @return a suitable substring
   */
  public static String substring(String string, int from, int to) {
    return string.substring(from, to);
  }

  /**
   * Returns a new string containing characters starting at the given UTF-16 index.
   *
   * @param string the string to trim
   * @param from number of characters to drop
   * @return a trimmed string
   */
  public static String drop_first(String string, int from) {
    return string.substring(from);
  }

  /**
   * Converts a string into an array of UTF-8 bytes.
   *
   * @param str the string to convert
   * @return the UTF-8 representation of the string.
   */
  public static byte[] get_bytes(String str) {
    return str.getBytes(StandardCharsets.UTF_8);
  }

  /**
   * Converts a string into an array of UTF-16 chars.
   *
   * @param str the string to convert
   * @return the UTF-16 character representation of the string.
   */
  public static char[] get_chars(String str) {
    return str.toCharArray();
  }

  /**
   * Converts a string into an array of Unicode codepoints.
   *
   * @param str the string to convert
   * @return the codepoints of the original string.
   */
  public static int[] get_codepoints(String str) {
    return str.codePoints().toArray();
  }

  /**
   * Splits the string on each occurrence of {@code sep}, returning the resulting substrings in an
   * array.
   *
   * @param str the string to split
   * @param sep the separator string
   * @return array of substrings of {@code str} contained between occurences of {@code sep}
   */
  public static String[] split_by_literal(String str, String sep) {
    return str.split(Pattern.quote(sep));
  }

  /**
   * Splits the string on each occurrence of UTF-8 whitespace, returning the resulting substrings in
   * an array.
   *
   * @param str the string to split
   * @return the array of substrings of {@code str}
   */
  public static String[] split_on_whitespace(String str) {
    return whitespace.split(str);
  }

  /**
   * Splits the string on each occurrence of UTF-8 vertical whitespace, returning the resulting
   * substrings in an array.
   *
   * @param str the string to split
   * @return the array of substrings of {@code str}
   */
  public static String[] split_on_lines(String str) {
    return vertical_space.split(str);
  }

  /**
   * Checks whether two strings are equal up to Unicode canonicalization.
   *
   * @param str1 the first string
   * @param str2 the second string
   * @return the result of comparison
   */
  public static boolean equals(String str1, Object str2) {
    if (str2 instanceof String) {
      return compare_normalized(str1, (String) str2) == 0;
    } else {
      return false;
    }
  }

  /**
   * Converts an array of codepoints into a string.
   *
   * @param codepoints the codepoints to convert
   * @return the resulting string
   */
  public static String from_codepoints(int[] codepoints) {
    return new String(codepoints, 0, codepoints.length);
  }

  /**
   * Converts an array of UTF-8 bytes into a string.
   *
   * @param bytes the bytes to convert
   * @return the resulting string
   */
  public static String from_utf_8(byte[] bytes) {
    return new String(bytes, StandardCharsets.UTF_8);
  }

  /**
   * Converts an array of UTF-16 characters into a string.
   *
   * @param chars the UTF-16 characters to convert
   * @return the resulting string
   */
  public static String from_chars(char[] chars) {
    return String.valueOf(chars);
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

  /**
   * Checks if {@code substring} is a substring of {@code string}.
   *
   * @param string the containing string.
   * @param substring the contained string.
   * @return whether {@code substring} is a substring of {@code string}.
   */
  public static boolean contains(String string, String substring) {
    // {@code StringSearch} does not handle empty strings as we would want, so we need these special
    // cases.
    if (substring.isEmpty()) return true;
    if (string.isEmpty()) return false;
    StringSearch searcher = new StringSearch(substring, string);
    return searcher.first() != StringSearch.DONE;
  }

  /**
   * Replaces all occurrences of {@code oldSequence} within {@code str} with {@code newSequence}.
   *
   * @param str the string to process
   * @param oldSequence the substring that is searched for and will be replaced
   * @param newSequence the string that will replace occurrences of {@code oldSequence}
   * @return {@code str} with all occurrences of {@code oldSequence} replaced with {@code
   *     newSequence}
   */
  public static String replace(String str, String oldSequence, String newSequence) {
    return str.replace(oldSequence, newSequence);
  }

  /**
   * Gets the length of char array of a string
   *
   * @param str the string to measure
   * @return length of the string
   */
  public static long char_length(String str) {
    return str.length();
  }

  /**
   * Find the first index of needle in the haystack
   *
   * @param haystack the string to search
   * @param needle the substring that is searched for
   * @return index of the first needle or -1 if not found.
   */
  public static long index_of(String haystack, String needle) {
    StringSearch search = new StringSearch(needle, haystack);
    int pos = search.first();
    return pos == StringSearch.DONE ? -1 : pos;
  }

  /**
   * Find the last index of needle in the haystack
   *
   * @param haystack the string to search
   * @param needle the substring that is searched for
   * @return index of the last needle or -1 if not found.
   */
  public static long last_index_of(String haystack, String needle) {
    StringSearch search = new StringSearch(needle, haystack);
    int pos = search.first();
    if (pos == StringSearch.DONE) {
      return -1;
    }

    for (int next = search.next(); next != StringSearch.DONE; next = search.next()) {
      pos = next;
    }

    return pos;
  }

  /**
   * Normalizes the string to its canonical Unicode form using NFD decomposition.
   *
   * <p>This is to ensure that things like accents are in a common format, i.e. `ś` gets decomposed
   * into `s` and a separate codepoint for the accent etc.
   */
  public static String normalize(String str) {
    return Normalizer2.getNFDInstance().normalize(str);
  }

  /**
   * Checks if the given string consists only of whitespace characters.
   *
   * @param str the string to check
   * @return {@code true} if {@code str} is only whitespace, otherwise {@code false}
   */
  public static boolean is_all_whitespace(String text) {
    return text.codePoints().allMatch(UCharacter::isUWhiteSpace);
  }
}
