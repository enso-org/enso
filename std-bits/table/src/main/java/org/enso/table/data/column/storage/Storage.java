package org.enso.table.data.column.storage;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicLong;
import org.enso.table.data.column.storage.type.StorageType;

/** An abstract representation of a data column. */
public abstract class Storage<T> implements ColumnStorage<T> {
  private static final AtomicLong atomicCounter = new AtomicLong(0);

  private final long uniqueKey = atomicCounter.incrementAndGet();

  @Override
  public long uniqueKey() {
    return uniqueKey;
  }

  @Override
  public abstract long getSize();

  @Override
  public abstract StorageType<T> getType();

  @Override
  public abstract boolean isNothing(long index);

  @Override
  public abstract T getItemBoxed(long index);

  @Override
  public Iterator<T> iterator() {
    return new Iterator<>() {
      private long index = -1;

      @Override
      public boolean hasNext() {
        return index + 1 < getSize();
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
