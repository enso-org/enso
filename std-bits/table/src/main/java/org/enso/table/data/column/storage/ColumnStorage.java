package org.enso.table.data.column.storage;

import java.util.Iterator;
import java.util.NoSuchElementException;
import org.enso.table.data.column.storage.type.StorageType;

/** Basic interface of a column storage. */
public interface ColumnStorage<T> extends Iterable<T> {
  /* Gets a unique key for the storage. This is used for internal caching. */
  long uniqueKey();

  /* Gets the size of the storage. */
  long getSize();

  /**
   * Address of the off-heap storage of data.
   *
   * @return {@code 0} if there are no data to share, otherwise the address of the data in a format
   *     appropraite for this storage {@link #getType()}.
   * @see #addressOfValidity
   */
  default long addressOfData() {
    return 0;
  }

  /**
   * Address of the off-heap storage of validity bitmap.
   *
   * @return {@code 0} if there are no bitmap information to share
   */
  default long addressOfValidity() {
    return 0;
  }

  /* Gets the value type of the storage. */
  StorageType<T> getType();

  /**
   * Checks whether the value at idx is Nothing.
   *
   * @param index – the index to check.
   * @return whether the value is Nothing.
   */
  default boolean isNothing(long index) {
    return getItemBoxed(index) == null;
  }

  /* Gets the value at a given index. */
  T getItemBoxed(long index);

  @Override
  default Iterator<T> iterator() {
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
