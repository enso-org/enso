package org.enso.table.util;

import java.util.Comparator;
import java.util.Locale;
import org.enso.base.Text_Utils;

/**
 * An {@link Comparator} for Text that ensures the same behaviour as Enso case-insensitive equality
 * (`equals_ignore_case`) on the Text type.
 */
final class CaseInsensitiveUnicodeNormalizedTextComparator implements Comparator<Object> {
  private final Locale locale;

  CaseInsensitiveUnicodeNormalizedTextComparator(Locale locale) {
    this.locale = locale;
  }

  @Override
  public int compare(Object a, Object b) {
    if (a instanceof String sa) {
      if (b instanceof String sb) {
        return Text_Utils.compare_normalized_ignoring_case(sa, sb, locale);
      }
    }

    throw new IllegalStateException(
        "CaseInsensitiveUnicodeNormalizedTextComparator can only compare Strings.");
  }
}
