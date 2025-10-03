package org.enso.table_test_helpers;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicLong;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.iterators.LongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;

/**
 * A helper class used in the Upload_Spec test to purposefully interrupt a table upload in the
 * middle of it by throwing an exception. It is used to test the transactionality of the upload.
 */
public class ExplodingStorage implements ColumnLongStorage {
  private static final AtomicLong atomicCounter = new AtomicLong(100000000);

  private final long uniqueKey = atomicCounter.incrementAndGet();
  private final long[] array;
  private final long explodingIndex;

  public ExplodingStorage(long[] array, long explodingIndex) {
    this.array = array;
    this.explodingIndex = explodingIndex;
  }

  private void checkIndex(long idx) {
    if (idx == explodingIndex) {
      throw new ExplodingStoragePayload();
    }
  }

  @Override
  public long uniqueKey() {
    return uniqueKey;
  }

  @Override
  public long getSize() {
    return array.length;
  }

  @Override
  public long getItemAsLong(long index) throws ValueIsNothingException {
    checkIndex(index);
    return array[Math.toIntExact(index)];
  }

  @Override
  public IntegerType getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean isNothing(long idx) {
    checkIndex(idx);
    return false;
  }

  @Override
  public Long getItemBoxed(long idx) {
    return getItemAsLong(idx);
  }

  @Override
  public Iterator<Long> iterator() {
    return new Iterator<>() {
      private long index = -1;

      @Override
      public boolean hasNext() {
        return index + 1 < getSize();
      }

      @Override
      public Long next() {
        if (!hasNext()) {
          throw new NoSuchElementException();
        }
        return getItemBoxed(++index);
      }
    };
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(this);
  }
}
