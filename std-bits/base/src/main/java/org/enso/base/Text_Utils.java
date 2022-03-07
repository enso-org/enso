package org.enso.base;

import com.ibm.icu.lang.UCharacter;
import com.ibm.icu.text.BreakIterator;
import com.ibm.icu.text.CaseMap;
import com.ibm.icu.text.CaseMap.Fold;
import com.ibm.icu.text.Normalizer;
import com.ibm.icu.text.Normalizer2;
import com.ibm.icu.text.StringSearch;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
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
   * Checks whether two strings are equal up to Unicode canonicalization and ignoring case.
   *
   * @param str1 the first string
   * @param str2 the second string
   * @param locale the locale to use for case folding
   * @return the result of comparison
   */
  public static boolean equals_ignore_case(String str1, Object str2, Locale locale) {
    if (str2 instanceof String) {
      Fold fold = case_fold_algorithm_for_locale(locale);
      return compare_normalized(fold.apply(str1), fold.apply((String) str2)) == 0;
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
   * Checks if {@code substring} is a substring of {@code string}.
   *
   * @param string the containing string.
   * @param substring the contained string.
   * @return whether {@code substring} is a substring of {@code string}.
   */
  public static boolean contains_case_insensitive(String string, String substring, Locale locale) {
    // {@code StringSearch} does not handle empty strings as we would want, so we need these special
    // cases.
    if (substring.length() == 0) return true;
    if (string.length() == 0) return false;

    Fold fold = case_fold_algorithm_for_locale(locale);
    StringSearch searcher = new StringSearch(fold.apply(substring), fold.apply(string));
    return searcher.first() != StringSearch.DONE;
  }

  /**
   * Transforms the provided string into a form which can be used for case insensitive comparisons.
   *
   * @param string the string to transform
   * @param locale the locale to use - needed to distinguish a special case when handling Turkish
   *     'i' characters
   * @return a transformed string that can be used for case insensitive comparisons
   */
  public static String case_insensitive_key(String string, Locale locale) {
    Fold fold = case_fold_algorithm_for_locale(locale);
    return fold.apply(string);
  }

  private static final Locale AZ_LOCALE = new Locale("az");
  private static final Locale TR_LOCALE = new Locale("tr");

  /** Returns a case folding algorithm appropriate for the given locale. */
  public static Fold case_fold_algorithm_for_locale(Locale locale) {
    if (locale.equals(AZ_LOCALE) || locale.equals(TR_LOCALE)) {
      return CaseMap.fold().turkic();
    }
    return CaseMap.fold();
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

  public static List<Long> index_of_all(String haystack, String needle) {
    StringSearch search = new StringSearch(needle, haystack);
    ArrayList<Long> occurrences = new ArrayList<>();
    long ix;
    while ((ix = search.next()) != StringSearch.DONE) {
      occurrences.add(ix);
    }
    return occurrences;
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
    int pos = search.last();
    if (pos == StringSearch.DONE) {
      return -1;
    }
    return pos;
  }

  /**
   * Converts a codepoint index to index of the grapheme that this codepoint belongs to.
   *
   * @param text the text associated with the index
   * @param codepoint_indices the codepoint index
   * @return a grapheme index corresponding to the codepoint from the input
   */
  public static long codepoint_index_to_grapheme_index(String text, long codepoint_index) {
    BreakIterator breakIterator = BreakIterator.getCharacterInstance();
    breakIterator.setText(text);
    if (codepoint_index < 0 || codepoint_index > text.length()) {
      throw new IndexOutOfBoundsException(
          "Index " + codepoint_index + " is outside of the provided text.");
    }

    int grapheme_end = breakIterator.next();
    int grapheme_index = 0;

    while (grapheme_end <= codepoint_index && grapheme_end != -1) {
      grapheme_index++;
      grapheme_end = breakIterator.next();
    }
    return grapheme_index;
  }

  /**
   * Converts a series of codepoint indices to indices of graphemes that these codepoints belong to.
   *
   * @param text the text associated with the indices
   * @param codepoint_indices the array of codepoint indices
   * @return an array of grapheme indices corresponding to the codepoints from the input
   */
  public static List<Long> codepoint_indices_to_grapheme_indices(
      String text, List<Long> codepoint_indices) {
    BreakIterator breakIterator = BreakIterator.getCharacterInstance();
    breakIterator.setText(text);
    // TODO
    return new ArrayList<>();
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
