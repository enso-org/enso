package org.enso.table.util;

import java.util.Comparator;
import org.enso.base.Text_Utils;

/**
 * An {@link Comparator} for Text that ensures the same behaviour as Enso equality (`==`) on the
 * Text type.
 */
final class UnicodeNormalizedTextComparator implements Comparator<Object> {
  @Override
  public int compare(Object a, Object b) {
    if (a instanceof String sa) {
      if (b instanceof String sb) {
        return Text_Utils.compare_normalized(sa, sb);
      }
    }

    throw new IllegalStateException("UnicodeNormalizedTextComparator can only compare Strings.");
  }

  static final UnicodeNormalizedTextComparator INSTANCE = new UnicodeNormalizedTextComparator();
}
