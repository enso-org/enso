package org.enso.table.data.column.storage;

import java.util.BitSet;

public interface ColumnStorageWithNothingMap extends ColumnStorage {
  /** Gets the isNothing map for the storage. */
  BitSet getIsNothingMap();
}
