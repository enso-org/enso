package org.enso.table.data.index;

import java.util.Objects;

import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;

/**
 * A multi-value key for unordered operations like group-by or distinct.
 *
 * <p>It relies on folding logic that coerces values to their representatives in such a way that
 * their equality is consistent with how Enso would handle equality.
 *
 * <p>As it relies on hashing, it currently is not prepared to work correctly for custom
 * Enso-defined objects, as hashing of such objects is not yet implemented properly.
 */
public class UnorderedMultiValueKey extends MultiValueKeyBase {
  private final int hashCodeValue;
  private final TextFoldingStrategy textFoldingStrategy;

  public UnorderedMultiValueKey(Storage[] storages, int rowIndex) {
    this(storages, rowIndex, TextFoldingStrategy.unicodeNormalizedFold);
  }

  public UnorderedMultiValueKey(
      Storage[] storages, int rowIndex, TextFoldingStrategy textFoldingStrategy) {
    super(storages, rowIndex);
    this.textFoldingStrategy = textFoldingStrategy;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 1;
    for (int i = 0; i < storages.length; i++) {
      h = 31 * h;

      Object value = this.get(i);
      if (value != null) {
        Object folded = foldObject(value);
        hasFloatValues = hasFloatValues || (folded instanceof Double);
        h += folded.hashCode();
      }
    }

    this.hashCodeValue = h;
    floatsComputed = true;
  }

  /**
   * Folds the value to ensure consistency with Enso's equality.
   *
   * <p>Case-sensitivity of text folding is controlled by {@code textFoldingStrategy}.
   */
  protected Object foldObject(Object value) {
    if (value instanceof String s) {
      return textFoldingStrategy.fold(s);
    } else {
      return foldNumeric(value);
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof UnorderedMultiValueKey that)) return false;
    if (storages.length != that.storages.length) return false;
    if (hashCodeValue != that.hashCodeValue) return false;
    for (int i = 0; i < storages.length; i++) {
      Object thisFolded = foldObject(this.get(i));
      Object thatFolded = foldObject(that.get(i));
      if (!Objects.equals(thisFolded, thatFolded)) {
        return false;
      }
    }

    return true;
  }

  @Override
  public int hashCode() {
    return this.hashCodeValue;
  }
}
