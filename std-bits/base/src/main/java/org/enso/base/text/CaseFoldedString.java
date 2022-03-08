package org.enso.base.text;

import com.ibm.icu.text.BreakIterator;
import com.ibm.icu.text.CaseMap;
import com.ibm.icu.text.CaseMap.Fold;
import java.util.Locale;

public class CaseFoldedString {
  private final String string;
  private final CharSequence original;
  private final int[] grapheme_index_mapping;

  private CaseFoldedString(String string, CharSequence original, int[] grapheme_index_mapping) {
    this.string = string;
    this.original = original;
    this.grapheme_index_mapping = grapheme_index_mapping;
  }

  public int codeUnitToGraphemeIndex(int codeunitIndex) {
    return grapheme_index_mapping[codeunitIndex];
  }

  public String getFoldedString() {
    return string;
  }

  /**
   * Folds a string remembering the mapping from code units to its original grapheme cluster
   * indices.
   */
  public static CaseFoldedString fold(CharSequence string, Locale locale) {
    BreakIterator breakIterator = BreakIterator.getCharacterInstance();
    StringBuilder stringBuilder = new StringBuilder(string.length());
    Fold foldAlgorithm = caseFoldAlgorithmForLocale(locale);
    IntArrayBuilder index_mapping = new IntArrayBuilder(string.length() + 1);

    // We rely on the fact that ICU Case Folding is _not_ context-sensitive, i.e. the mapping of
    // each grapheme cluster is independent of surrounding ones. Regular casing is
    // context-sensitive.
    int current = breakIterator.current();
    int next;
    int grapheme_index = 0;
    while ((next = breakIterator.next()) != BreakIterator.DONE) {
      CharSequence grapheme = new StringSlice(string, current, next);
      String foldedGrapheme = foldAlgorithm.apply(grapheme);
      stringBuilder.append(foldedGrapheme);
      for (int i = 0; i < foldedGrapheme.length(); ++i) {
        index_mapping.add(grapheme_index);
      }
      grapheme_index++;
    }

    // The mapping should also be able to handle a {@code str.length()} query, so we add one more
    // element to the mapping pointing to a non-existent grapheme after the end of the text.
    index_mapping.add(grapheme_index);

    return new CaseFoldedString(stringBuilder.toString(), string, index_mapping.unsafeGetStorage());
  }

  public static String simpleFold(CharSequence string, Locale locale) {
    return caseFoldAlgorithmForLocale(locale).apply(string);
  }

  private static final Locale AZ_LOCALE = new Locale("az");
  private static final Locale TR_LOCALE = new Locale("tr");

  /** Returns a case folding algorithm appropriate for the given locale. */
  public static Fold caseFoldAlgorithmForLocale(Locale locale) {
    if (locale.equals(AZ_LOCALE) || locale.equals(TR_LOCALE)) {
      return CaseMap.fold().turkic();
    }
    return CaseMap.fold();
  }
}
