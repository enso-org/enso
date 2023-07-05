package org.enso.table.data.index;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.enso.base.polyglot.EnsoObjectWrapper;
import org.enso.base.polyglot.NumericConverter;
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
  private final List<TextFoldingStrategy> textFoldingStrategy;

  public UnorderedMultiValueKey(
      Storage<?>[] storages, int rowIndex, List<TextFoldingStrategy> textFoldingStrategy) {
    super(storages, rowIndex);
    this.textFoldingStrategy = textFoldingStrategy;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 1;
    for (int i = 0; i < storages.length; i++) {
      h = 31 * h;

      Object value = this.get(i);
      if (value != null) {
        hasFloatValues = hasFloatValues || NumericConverter.isDecimalLike(value);
        Object folded = EnsoObjectWrapper.foldObject(value, textFoldingStrategy.get(i));
        h += folded.hashCode();
      }
    }

    this.hashCodeValue = h;
    floatsComputed = true;
  }

  protected Object getObjectFolded(int index) {
    return EnsoObjectWrapper.foldObject(this.get(index), textFoldingStrategy.get(index));
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof UnorderedMultiValueKey that)) return false;
    if (storages.length != that.storages.length) return false;
    if (hashCodeValue != that.hashCodeValue) return false;
    for (int i = 0; i < storages.length; i++) {
      Object thisFolded = this.getObjectFolded(i);
      Object thatFolded = that.getObjectFolded(i);
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

  @Override
  public String toString() {
    return "UnorderedMultiValueKey{"
        + "hashCode="
        + hashCodeValue
        + ", values="
        + IntStream.range(0, storages.length)
            .mapToObj(i -> String.valueOf(this.get(i)))
            .collect(Collectors.joining(", "))
        + '}';
  }
}
