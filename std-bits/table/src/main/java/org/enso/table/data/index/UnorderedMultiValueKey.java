package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.text.TextFoldingStrategy;
import org.enso.table.text.UnicodeNormalizedFold;

import java.util.Objects;

public class UnorderedMultiValueKey extends MultiValueKeyBase {
  private final int hashCodeValue;
  private final TextFoldingStrategy textFoldingStrategy;

  public UnorderedMultiValueKey(Storage[] storages, int rowIndex) {
    this(storages, rowIndex, UnicodeNormalizedFold.INSTANCE);
  }

  public UnorderedMultiValueKey(Storage[] storages, int rowIndex, TextFoldingStrategy textFoldingStrategy) {
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

  @Override
  protected Object foldObject(Object value) {
    if (value instanceof String s) {
      return textFoldingStrategy.fold(s);
    } else {
      return super.foldObject(value);
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof MultiValueKeyBase that)) return false;
    if (storages.length != that.storages.length) return false;
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
