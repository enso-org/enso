package org.enso.table.data.column.storage;

public interface ColumnDoubleStorage extends ColumnStorage<Double> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  double getItemDouble(long index) throws ValueIsNothingException;
}
