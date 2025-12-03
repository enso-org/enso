package org.enso.table.data.column.storage;

import org.enso.table.util.ImmutableBitSet;

public interface ColumnStorageWithValidityMap {
  /**
   * Gets the validity map for the storage.
   *
   * @return bit set with {@code false} at null indexes and {@code true} at non-null indexes
   */
  ImmutableBitSet getValidityMap();
}
