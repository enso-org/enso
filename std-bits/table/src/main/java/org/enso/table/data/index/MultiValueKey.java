package org.enso.table.data.index;

import java.util.Arrays;

public class MultiValueKey {
  private final Object[] values;
  private final int hashCodeValue;

  public MultiValueKey(Object[] values) {
    this.values = values;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 0;
    for (Object value: this.values) {
      if (value != null) {
        h ^= value.hashCode();
      }
    }
    h += ~(h << 9);
    h ^= h >>> 14;
    h += h << 4;
    this.hashCodeValue = h ^ (h >>> 10);
  }

  @Override
  public int hashCode() {
    return this.hashCodeValue;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    MultiValueKey that = (MultiValueKey) o;
    return hashCodeValue == that.hashCodeValue && Arrays.equals(values, that.values);
  }
}
