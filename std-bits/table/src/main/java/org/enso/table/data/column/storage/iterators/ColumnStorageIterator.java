package org.enso.table.data.column.storage.iterators;

import java.util.Iterator;

public interface ColumnStorageIterator<T> extends Iterator<T> {
  // Gets the current item.
  T getItemBoxed();

  // Checks whether the value at idx is Nothing.
  boolean isNothing();

  // Gets the current index;
  long getIndex();

  // Moves to the next item.
  // Returns true if not finished, false otherwise.
  boolean moveNext();
}
