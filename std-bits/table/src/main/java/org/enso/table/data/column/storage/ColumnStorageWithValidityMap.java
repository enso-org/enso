package org.enso.table.data.column.storage;

import java.util.BitSet;

public interface ColumnStorageWithValidityMap {
  /**
   * Gets the isNothing map for the storage.
   *
   * @return bit set with {@code false} at null indexes and {@code true} at non-null indexes
   */
  BitSet getValidityMap();
}
