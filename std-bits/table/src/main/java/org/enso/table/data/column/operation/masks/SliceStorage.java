package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.StorageType;

import java.util.Iterator;
import java.util.NoSuchElementException;

class SliceStorage<T> implements ColumnStorage<T> {
  private final ColumnStorage<T> parent;
  protected final long start;
  protected final long end;

  public SliceStorage(ColumnStorage<T> parent, long start, long end) {
    this.parent = parent;
    this.start = start;
    this.end = end;
  }

  ColumnStorage<T> parent() {
    return parent;
  }

  @Override
  public long uniqueKey() {
    return parent.uniqueKey();
  }

  @Override
  public long getSize() {
    return 0;
  }

  @Override
  public StorageType<T> getType() {
    return parent.getType();
  }

  @Override
  public boolean isNothing(long index) {
    if (index < 0 || index >= end - start) {
      throw new IndexOutOfBoundsException(index);
    }
    return parent.isNothing(index - start);
  }

  @Override
  public T getItemBoxed(long index) {
    if (index < 0 || index >= end - start) {
      throw new IndexOutOfBoundsException(index);
    }
    return parent.getItemBoxed(index - start);
  }

  @Override
  public Iterator<T> iterator() {
    return new Iterator<>() {
      private long index = -1;

      @Override
      public boolean hasNext() {
        return index + 1 < end;
      }

      @Override
      public T next() {
        if (!hasNext()) {
          throw new NoSuchElementException();
        }
        return getItemBoxed(++index);
      }
    };
  }
}

