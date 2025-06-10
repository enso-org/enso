package org.enso.table.data.column.storage;

public interface ColumnBooleanStorageIterator extends ColumnStorageIterator<Boolean> {
  /** Gets the current item as a boolean. Note if the item isNothing value is undefined. */
  boolean getItemAsBoolean();
}
