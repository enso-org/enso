package org.enso.table.data.column.storage.iterators;

import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.ColumnStorage;

public sealed class AbstractBaseIterator<T> implements ColumnStorageIterator<T>
    permits BooleanStorageIterator, DoubleStorageIterator, LongStorageIterator {
  protected final ColumnStorage<T> parent;
  private long index;
  private final long length;

  protected AbstractBaseIterator(ColumnStorage<T> parent) {
    this.parent = parent;
    this.index = -1; // Start at one before the first item
    this.length = parent.getSize();
  }

  @Override
  public T getItemBoxed() {
    return parent.getItemBoxed(index);
  }

  @Override
  public boolean isNothing() {
    return parent.isNothing(index);
  }

  @Override
  public boolean hasNext() {
    return index + 1 < length;
  }

  @Override
  public T next() {
    if (!hasNext()) {
      throw new NoSuchElementException();
    }
    return parent.getItemBoxed(++index);
  }

  @Override
  public long getIndex() {
    return index;
  }

  @Override
  public boolean moveNext() {
    if (!hasNext()) {
      return false;
    }
    index++;
    return true;
  }
}
