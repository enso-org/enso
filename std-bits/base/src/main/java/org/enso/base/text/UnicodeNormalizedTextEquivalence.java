package org.enso.base.text;

import org.enso.base.Text_Utils;
import org.graalvm.collections.Equivalence;

/**
 * An {@link Equivalence} for Text that ensures the same behaviour as Enso equality (`==`) on the Text type.
 */
public class UnicodeNormalizedTextEquivalence extends Equivalence {
  @Override
  public boolean equals(Object a, Object b) {
    if (a instanceof String sa) {
      if (b instanceof String sb) {
        return Text_Utils.equals(sa, sb);
      }
    }

    throw new IllegalStateException("UnicodeNormalizedTextEquivalence can only compare Strings.");
  }

  @Override
  public int hashCode(Object o) {
    if (o instanceof String s) {
      return Text_Utils.unicodeNormalizedHashCode(s);
    }

    throw new IllegalStateException("UnicodeNormalizedTextEquivalence can only hash Strings.");
  }

  public static final UnicodeNormalizedTextEquivalence INSTANCE = new UnicodeNormalizedTextEquivalence();
}
