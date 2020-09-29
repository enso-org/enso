package org.enso.base;

import com.ibm.icu.text.Normalizer2;

import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/** Utils for standard library operations on Text. */
public class Text_Utils {
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
   * Converts a string into an array of UTF-8 bytes.
   *
   * @param str the string to convert
   * @return the UTF-8 representation of the string.
   */
  public static byte[] get_bytes(String str) {
    return str.getBytes(StandardCharsets.UTF_8);
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
  public static String[] split_at(String str, String sep) {
    return str.split(Pattern.quote(sep));
  }

  /**
   * Checks whether two strings are equal up to Unicode canonicalization.
   *
   * @param str1 the first string
   * @param str2 the second string
   * @return the result of comparison
   */
  public static boolean equals(String str1, String str2) {
    return Normalizer2.getNFDInstance()
        .normalize(str1)
        .equals(Normalizer2.getNFDInstance().normalize(str2));
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
}
