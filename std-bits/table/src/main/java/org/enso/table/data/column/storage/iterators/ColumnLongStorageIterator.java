package org.enso.table.data.column.storage.iterators;

public interface ColumnLongStorageIterator extends ColumnStorageIterator<Long> {
  /** Gets the current item as a long. Note if the item isNothing value is undefined. */
  long getItemAsLong();
}
