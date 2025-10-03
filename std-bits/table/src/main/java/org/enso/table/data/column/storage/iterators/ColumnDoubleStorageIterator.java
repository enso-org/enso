package org.enso.table.data.column.storage.iterators;

public interface ColumnDoubleStorageIterator extends ColumnStorageIterator<Double> {
  /** Gets the current item as a double. Note if the item isNothing value is undefined. */
  double getItemAsDouble();
}
