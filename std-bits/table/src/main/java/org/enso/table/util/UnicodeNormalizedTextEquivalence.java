package org.enso.table.util;

import java.util.Comparator;
import org.enso.base.Text_Utils;

/**
 * An {@link Equivalence} for Text that ensures the same behaviour as Enso equality (`==`) on the
 * Text type.
 */
final class UnicodeNormalizedTextEquivalence implements Comparator<Object> {
  @Override
  public int compare(Object a, Object b) {
    if (a instanceof String sa) {
      if (b instanceof String sb) {
        return Text_Utils.compare_normalized(sa, sb);
      }
    }

    throw new IllegalStateException("UnicodeNormalizedTextEquivalence can only compare Strings.");
  }

  static final UnicodeNormalizedTextEquivalence INSTANCE = new UnicodeNormalizedTextEquivalence();
}
