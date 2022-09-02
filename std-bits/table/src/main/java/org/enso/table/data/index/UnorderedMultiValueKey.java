package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;

import java.util.Objects;

public class UnorderedMultiValueKey extends MultiValueKeyBase {
  private final int hashCodeValue;

  public UnorderedMultiValueKey(Storage[] storages, int rowIndex) {
    super(storages, rowIndex);

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
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof MultiValueKeyBase that)) return false;
    if (storages.length != that.storages.length) return false;
    for (int i = 0; i < storages.length; i++) {
      if (!Objects.equals(get(i), that.get(i))) {
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
