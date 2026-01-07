package org.enso.table.util;

import java.util.Locale;
import org.enso.base.Text_Utils;
import org.graalvm.collections.Equivalence;

/**
 * An {@link Equivalence} for Text that ensures the same behaviour as Enso case-insensitive equality
 * (`equals_ignore_case`) on the Text type.
 */
final class CaseInsensitiveUnicodeNormalizedTextEquivalence extends Equivalence {
  private final Locale locale;

  CaseInsensitiveUnicodeNormalizedTextEquivalence(Locale locale) {
    this.locale = locale;
  }

  public boolean equals(Object a, Object b) {
    if (a instanceof String sa) {
      if (b instanceof String sb) {
        return Text_Utils.equals_ignore_case(sa, sb, locale);
      }
    }

    throw new IllegalStateException("UnicodeNormalizedTextEquivalence can only compare Strings.");
  }

  public int hashCode(Object o) {
    if (o instanceof String s) {
      String keyed = Text_Utils.case_insensitive_key(s, locale);
      return Text_Utils.unicodeNormalizedHashCode(keyed);
    }

    throw new IllegalStateException(
        "CaseInsensitiveUnicodeNormalizedTextEquivalence can only hash Strings.");
  }
}
