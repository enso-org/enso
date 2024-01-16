package org.enso.base;

import com.ibm.icu.lang.UCharacter;
import com.ibm.icu.text.BreakIterator;
import com.ibm.icu.text.CaseMap.Fold;
import com.ibm.icu.text.Normalizer;
import com.ibm.icu.text.Normalizer2;
import com.ibm.icu.text.Normalizer2.Mode;
import com.ibm.icu.text.StringSearch;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.base.text.CaseFoldedString;
import org.enso.base.text.CaseFoldedString.Grapheme;
import org.enso.base.text.GraphemeSpan;
import org.enso.base.text.Utf16Span;
import org.enso.polyglot.common_utils.Core_Text_Utils;
import org.graalvm.polyglot.Context;

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
   * Converts a string into an array of UTF-16 chars.
   *
   * @param str the string to convert
   * @return the UTF-16 character representation of the string.
   */
  public static int[] get_chars(String str) {
    return str.chars().toArray();
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
   * Splits the string on each occurrence of UTF-8 vertical whitespace, returning the resulting
   * substrings in an array.
   *
   * @param str the string to split
   * @param keep_endings whether to keep line endings in returned lines
   * @return the array of substrings of {@code str}
   */
  public static List<String> split_on_lines(String str, boolean keep_endings) {
    ArrayList<String> acc = new ArrayList<>();
    int length = str.length();
    int currentStart = 0;
    int currentPos = 0;
    Context context = Context.getCurrent();
    while (currentPos < length) {
      if (str.charAt(currentPos) == '\n') {
        acc.add(str.substring(currentStart, keep_endings ? currentPos + 1 : currentPos));
        currentStart = currentPos + 1;
        currentPos = currentStart;
      } else if (str.charAt(currentPos) == '\r') {
        // Handle the '\r\n' digraph.
        int offset = 1;
        if (currentPos + 1 < length && str.charAt(currentPos + 1) == '\n') {
          offset = 2;
        }
        acc.add(str.substring(currentStart, keep_endings ? currentPos + offset : currentPos));
        currentStart = currentPos + offset;
        currentPos = currentStart;
      } else {
        currentPos += 1;
      }

      context.safepoint();
    }

    if (currentStart < length) {
      acc.add(str.substring(currentStart));
    }

    return acc;
  }

  /**
   * Checks whether two strings are equal up to Unicode canonicalization.
   *
   * @param str1 the first string
   * @param str2 the second string
   * @return the result of comparison
   */
  public static boolean equals(String str1, String str2) {
    return Core_Text_Utils.equals(str1, str2);
  }

  /** Computes a hashcode of a string that is insensitive to Unicode normalization. */
  public static int unicodeNormalizedHashCode(String str) {
    return Core_Text_Utils.unicodeNormalizedHashCode(str);
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
    if (str2 instanceof String string2) {
      return compare_normalized_ignoring_case(str1, string2, locale) == 0;
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
    return Core_Text_Utils.compare_normalized(a, b);
  }

  /**
   * Compares {@code a} to {@code b} according to the lexicographical order, with optional Unicode
   * normalization depending on the {@code isNormalized} parameter.
   *
   * @param a the left operand
   * @param b the right operand
   * @param isNormalized {@code true} if both input strings are normalized
   * @return a negative value if {@code a} is before {@code b}, 0 if both values are equal and a
   *     positive value if {@code a} is after {@code b}
   */
  public static int compare(String a, String b, boolean isNormalized) {
    int options =
        isNormalized
            ? Normalizer.FOLD_CASE_DEFAULT | Normalizer.INPUT_IS_FCD
            : Normalizer.FOLD_CASE_DEFAULT;
    return Normalizer.compare(a, b, options);
  }

  /**
   * Compares {@code a} to {@code b} according to the lexicographical order, handling Unicode
   * normalization.
   *
   * @param a the left operand
   * @param b the right operand
   * @param locale the locale to use for case folding
   * @return a negative value if {@code a} is before {@code b}, 0 if both values are equal and a
   *     positive value if {@code a} is after {@code b}
   */
  public static int compare_normalized_ignoring_case(String a, String b, Locale locale) {
    Fold fold = CaseFoldedString.caseFoldAlgorithmForLocale(locale);
    return Normalizer.compare(fold.apply(a), fold.apply(b), Normalizer.FOLD_CASE_DEFAULT);
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

  /** Checks if {@code string} starts with {@code prefix}. */
  public static boolean starts_with(String string, String prefix) {
    String beginning = take_prefix(string, grapheme_length(prefix));
    return equals(beginning, prefix);
  }

  /** Checks if {@code string} ends with {@code suffix}. */
  public static boolean ends_with(String string, String suffix) {
    String ending = take_suffix(string, grapheme_length(suffix));
    return equals(ending, suffix);
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
    if (substring.isEmpty()) return true;
    if (string.isEmpty()) return false;

    Fold fold = CaseFoldedString.caseFoldAlgorithmForLocale(locale);
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
    return CaseFoldedString.simpleFold(string, locale);
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
   * Gets the length of the string in graphemes
   *
   * @param str the string to measure
   * @return length of the string
   */
  public static long grapheme_length(String str) {
    return Core_Text_Utils.computeGraphemeLength(str);
  }

  /** Returns a prefix of the string not exceeding the provided grapheme length. */
  public static String take_prefix(String str, long grapheme_length) {
    return Core_Text_Utils.take_prefix(str, grapheme_length);
  }

  /** Returns a suffix of the string not exceeding the provided grapheme length. */
  public static String take_suffix(String str, long grapheme_length) {
    BreakIterator iter = BreakIterator.getCharacterInstance();
    iter.setText(str);
    iter.last();
    if (grapheme_length <= 0) {
      return "";
    } else if (iter.next(Math.toIntExact(-grapheme_length)) == BreakIterator.DONE) {
      return str;
    } else {
      return str.substring(iter.current());
    }
  }

  /**
   * Find the first occurrence of needle in the haystack
   *
   * @param haystack the string to search
   * @param needle the substring that is searched for
   * @return a UTF-16 code unit span of the first needle or null if not found.
   */
  public static Utf16Span span_of(String haystack, String needle) {
    if (needle.isEmpty()) return new Utf16Span(0, 0);
    if (haystack.isEmpty()) return null;

    StringSearch search = new StringSearch(needle, haystack);
    int pos = search.first();
    if (pos == StringSearch.DONE) return null;
    return new Utf16Span(pos, pos + search.getMatchLength());
  }

  /**
   * Find the last occurrence of needle in the haystack
   *
   * @param haystack the string to search
   * @param needle the substring that is searched for
   * @return a UTF-16 code unit span of the last needle or null if not found.
   */
  public static Utf16Span last_span_of(String haystack, String needle) {
    if (needle.isEmpty()) {
      int afterLast = haystack.length();
      return new Utf16Span(afterLast, afterLast);
    }
    if (haystack.isEmpty()) return null;

    StringSearch search = new StringSearch(needle, haystack);
    int pos = search.last();
    if (pos == StringSearch.DONE) return null;
    return new Utf16Span(pos, pos + search.getMatchLength());
  }

  /**
   * Find spans of all occurrences of the needle within the haystack.
   *
   * @param haystack the string to search
   * @param needle the substring that is searched for
   * @return a list of UTF-16 code unit spans at which the needle occurs in the haystack
   */
  public static List<Utf16Span> span_of_all(String haystack, String needle) {
    if (needle.isEmpty())
      throw new IllegalArgumentException(
          "The operation `span_of_all` does not support searching for an empty term.");
    if (haystack.isEmpty()) return List.of();

    StringSearch search = new StringSearch(needle, haystack);
    ArrayList<Utf16Span> occurrences = new ArrayList<>();
    int ix;
    Context context = Context.getCurrent();
    while ((ix = search.next()) != StringSearch.DONE) {
      occurrences.add(new Utf16Span(ix, ix + search.getMatchLength()));
      context.safepoint();
    }
    return occurrences;
  }

  /**
   * Find spans of all occurrences of a set of needles within the haystack.
   *
   * @param haystack the string to search
   * @param needles the substrings that are searched for
   * @return a list of UTF-16 code unit spans at which the needle occurs in the haystack
   */
  public static List<Utf16Span> span_of_all_multiple(String haystack, List<String> needles) {
    if (needles.isEmpty() || needles.stream().anyMatch(String::isEmpty))
      throw new IllegalArgumentException(
          "The operation `span_of_all_multiple` does not support searching for an empty term.");
    if (haystack.isEmpty()) return List.of();

    StringSearch stringSearches[] =
        IntStream.range(0, needles.size())
            .mapToObj(i -> new StringSearch(needles.get(i), haystack))
            .toArray(StringSearch[]::new);
    List<Utf16Span> occurrences = new ArrayList<>();

    Context context = Context.getCurrent();

    int ix = 0;
    while (ix != StringSearch.DONE) {
      int earliestIndex = -1;
      int earliestStart = -1;
      for (int i = 0; i < stringSearches.length; ++i) {
        StringSearch stringSearch = stringSearches[i];
        int start = stringSearch.following(ix);
        if (start != StringSearch.DONE && (earliestStart == -1 || start < earliestStart)) {
          earliestIndex = i;
          earliestStart = start;
        }

        context.safepoint();
      }
      if (earliestIndex == -1) {
        // No more matches.
        break;
      }
      int matchLength = stringSearches[earliestIndex].getMatchLength();
      occurrences.add(new Utf16Span(earliestStart, earliestStart + matchLength));
      ix = earliestStart + matchLength;

      context.safepoint();
    }

    return occurrences;
  }

  /**
   * Converts a UTF-16 code unit index to index of the grapheme that this code unit belongs to.
   *
   * @param text the text associated with the index
   * @param codeunit_index the UTF-16 index
   * @return an index of an extended grapheme cluster that contains the code unit from the input
   */
  public static long utf16_index_to_grapheme_index(String text, long codeunit_index) {
    BreakIterator breakIterator = BreakIterator.getCharacterInstance();
    breakIterator.setText(text);
    if (codeunit_index < 0 || codeunit_index > text.length()) {
      throw new IndexOutOfBoundsException(
          "Index " + codeunit_index + " is outside of the provided text.");
    }

    int grapheme_end = breakIterator.next();
    long grapheme_index = 0;

    Context context = Context.getCurrent();
    while (grapheme_end <= codeunit_index && grapheme_end != BreakIterator.DONE) {
      grapheme_index++;
      grapheme_end = breakIterator.next();
      context.safepoint();
    }
    return grapheme_index;
  }

  /**
   * Converts a series of UTF-16 code unit indices to indices of graphemes that these code units
   * belong to.
   *
   * <p>For performance, it assumes that the provided indices are sorted in a non-decreasing order
   * (duplicate entries are permitted). Behaviour is unspecified if an unsorted list is provided.
   *
   * <p>The behaviour is unspecified if indices provided on the input are outside of the range [0,
   * text.length()].
   *
   * @param text the text associated with the indices
   * @param codeunit_indices the array of UTF-16 code unit indices, sorted in non-decreasing order
   * @return an array of grapheme indices corresponding to the UTF-16 units from the input
   */
  public static long[] utf16_indices_to_grapheme_indices(String text, List<Long> codeunit_indices) {
    BreakIterator breakIterator = BreakIterator.getCharacterInstance();
    breakIterator.setText(text);

    int grapheme_end = breakIterator.next();
    long grapheme_index = 0;

    long[] result = new long[codeunit_indices.size()];
    int result_ix = 0;

    Context context = Context.getCurrent();

    for (long codeunit_index : codeunit_indices) {
      while (grapheme_end <= codeunit_index && grapheme_end != BreakIterator.DONE) {
        grapheme_index++;
        grapheme_end = breakIterator.next();
        context.safepoint();
      }
      result[result_ix++] = grapheme_index;
    }

    return result;
  }

  /**
   * Find the first or last occurrence of needle in the haystack.
   *
   * @param haystack the string to search
   * @param needle the substring that is searched for
   * @param locale the locale used for case-insensitive comparisons
   * @param searchForLast if set to true, will search for the last occurrence; otherwise searches
   *     for the first one
   * @return an extended-grapheme-cluster span of the first or last needle, or null if none found.
   */
  public static GraphemeSpan span_of_case_insensitive(
      String haystack, String needle, Locale locale, boolean searchForLast) {
    if (needle.isEmpty())
      throw new IllegalArgumentException(
          "The operation `span_of_case_insensitive` does not support searching for an empty term.");
    if (haystack.isEmpty()) return null;

    CaseFoldedString foldedHaystack = CaseFoldedString.fold(haystack, locale);
    String foldedNeedle = CaseFoldedString.simpleFold(needle, locale);
    StringSearch search = new StringSearch(foldedNeedle, foldedHaystack.getFoldedString());
    int pos;
    if (searchForLast) {
      pos = search.last();
    } else {
      pos = search.first();
    }
    if (pos == StringSearch.DONE) {
      return null;
    } else {
      return findExtendedSpan(foldedHaystack, pos, search.getMatchLength());
    }
  }

  /**
   * Find all occurrences of needle in the haystack, case-insensitively.
   *
   * @param haystack the string to search
   * @param needles the substrings that are searched for
   * @param locale the locale used for case-insensitive comparisons
   * @return a list of extended-grapheme-cluster spans at which the needle occurs in the haystack
   */
  public static List<GraphemeSpan> span_of_all_case_insensitive(
      String haystack, String needle, Locale locale) {
    if (needle.isEmpty())
      throw new IllegalArgumentException(
          "The operation `span_of_all_case_insensitive` does not support searching for an empty"
              + " term.");
    if (haystack.isEmpty()) return List.of();

    CaseFoldedString foldedHaystack = CaseFoldedString.fold(haystack, locale);
    String foldedNeedle = CaseFoldedString.simpleFold(needle, locale);

    StringSearch search = new StringSearch(foldedNeedle, foldedHaystack.getFoldedString());
    ArrayList<GraphemeSpan> result = new ArrayList<>();

    int pos;
    Context context = Context.getCurrent();
    while ((pos = search.next()) != StringSearch.DONE) {
      result.add(findExtendedSpan(foldedHaystack, pos, search.getMatchLength()));
      context.safepoint();
    }

    return result;
  }

  /**
   * Find spans of all occurrences of a set of needles within the haystack, case-insensitively.
   *
   * @param haystack the string to search
   * @param needle the substring that is searched for
   * @param locale the locale used for case-insensitive comparisons
   * @return a list of extended-grapheme-cluster spans at which the needle occurs in the haystack
   */
  public static List<GraphemeSpan> span_of_all_case_insensitive_multiple(
      String haystack, List<String> needles, Locale locale) {
    CaseFoldedString foldedHaystack = CaseFoldedString.fold(haystack, locale);
    List<String> foldedNeedles =
        IntStream.range(0, needles.size())
            .mapToObj(i -> CaseFoldedString.simpleFold(needles.get(i), locale))
            .collect(Collectors.toList());
    var foldedSpans = span_of_all_multiple(foldedHaystack.getFoldedString(), foldedNeedles);
    List<GraphemeSpan> occurrences =
        foldedSpans.stream()
            .map(
                span ->
                    findExtendedSpan(
                        foldedHaystack,
                        span.codeunit_start,
                        span.codeunit_end - span.codeunit_start))
            .collect(Collectors.toList());
    return occurrences;
  }

  /**
   * Finds the grapheme span corresponding to the found match indexed with code units.
   *
   * <p>It extends the found span to ensure that graphemes associated with all found code units are
   * included in the resulting span. Thus, some additional code units which were not present in the
   * original match may also be present due to the extension.
   *
   * <p>The extension to the left is trivial - we just find the grapheme associated with the first
   * code unit and even if that code unit is not the first one of that grapheme, by returning it we
   * correctly extend to the left. The extension to the right works by finding the index of the
   * grapheme associated with the last code unit actually present in the span, then the end of the
   * returned span is set to the next grapheme after it. This correctly handles the edge case where
   * only a part of some grapheme was matched.
   *
   * @param string the folded string with which the positions are associated, containing a cache of
   *     position mappings
   * @param position the position of the match (in code units)
   * @param length the length of the match (in code units)
   * @return a minimal {@code GraphemeSpan} which contains all code units from the match
   */
  private static GraphemeSpan findExtendedSpan(CaseFoldedString string, int position, int length) {
    Grapheme firstGrapheme = string.findGrapheme(position);
    if (length == 0) {
      return new GraphemeSpan(
          firstGrapheme.index,
          firstGrapheme.index,
          firstGrapheme.codeunit_start,
          firstGrapheme.codeunit_start);
    } else {
      Grapheme lastGrapheme = string.findGrapheme(position + length - 1);
      int endGraphemeIndex = lastGrapheme.index + 1;
      return new GraphemeSpan(
          firstGrapheme.index,
          endGraphemeIndex,
          firstGrapheme.codeunit_start,
          lastGrapheme.codeunit_end);
    }
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
   * Normalizes the string to its canonical Unicode form using the specified name and mode.
   *
   * @see https://unicode-org.github.io/icu-docs/apidoc/dev/icu4j/com/ibm/icu/text/Normalizer2.html
   * @see
   *     https://unicode-org.github.io/icu-docs/apidoc/dev/icu4j/com/ibm/icu/text/Normalizer2.Mode.html
   * @param name the normalization name, must be "nfc", "nfkc", or "nfkc_cf"
   * @param mode the normalization mode
   */
  public static String normalizeWithMode(String str, String name, Mode mode) {
    return Normalizer2.getInstance(null, name, mode).normalize(str);
  }

  /**
   * Checks if the given string consists only of whitespace characters.
   *
   * @param text the string to check
   * @return {@code true} if {@code str} is only whitespace, otherwise {@code false}
   */
  public static boolean is_all_whitespace(String text) {
    return text.codePoints().allMatch(UCharacter::isUWhiteSpace);
  }

  /** Checks if the given string consists only of letters. */
  public static boolean is_all_letters(String text) {
    return text.codePoints().allMatch(UCharacter::isLetter);
  }

  /**
   * Replaces all provided spans within the text with {@code newSequence}.
   *
   * @param str the string to process
   * @param spans the spans to replace; the spans should be sorted by their starting point in the
   *     non-decreasing order; the behaviour is undefined if these requirements are not satisfied.
   * @param newSequence the string that will replace the spans
   * @return {@code str} with all provided spans replaced with {@code newSequence}
   */
  public static String replace_spans(String str, List<Utf16Span> spans, String newSequence) {
    StringBuilder sb = new StringBuilder();
    Context context = Context.getCurrent();
    int current_ix = 0;
    for (Utf16Span span : spans) {
      if (span.codeunit_start > current_ix) {
        sb.append(str, current_ix, span.codeunit_start);
      }

      sb.append(newSequence);
      current_ix = span.codeunit_end;
      context.safepoint();
    }

    // Add the remaining part of the string (if any).
    sb.append(str, current_ix, str.length());
    return sb.toString();
  }

  /** Pretty prints the string, escaping special characters. */
  public static String pretty_print(String str) {
    return Core_Text_Utils.prettyPrint(str);
  }
}
