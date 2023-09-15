package org.enso.base.text;

import java.util.Locale;
import org.enso.base.Text_Utils;

/** A strategy for folding text values for comparison and hashing. */
public interface TextFoldingStrategy {
  String fold(String value);

  /**
   * A folding strategy that ensures the strings are normalized, so various equivalent Unicode forms
   * are equated.
   */
  TextFoldingStrategy unicodeNormalizedFold = Text_Utils::normalize;

  /**
   * A folding strategy that not only normalizes the Unicode strings but also ensures
   * case-insensitive comparison. It needs a locale for locale-specific case handling.
   */
  static TextFoldingStrategy caseInsensitiveFold(Locale locale) {
    return (String value) -> {
      String normalized = Text_Utils.normalize(value);
      return Text_Utils.case_insensitive_key(normalized, locale);
    };
  }
}
