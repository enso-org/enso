package org.enso.table.data.column.storage;

import java.util.BitSet;

public interface ColumnStorageWithNothingMap {
  /** Gets the isNothing map for the storage. */
  BitSet getIsNothingMap();
}
