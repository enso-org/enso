package org.enso.table.data.column.storage;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Iterator;
import java.util.NoSuchElementException;

/** Basic interface of a column storage. */
public interface ColumnStorage<T> extends Iterable<T> {
  /* Gets a unique key for the storage. This is used for internal caching. */
  long uniqueKey();

  /* Gets the size of the storage. */
  long getSize();

  /* Gets the type character of the storage. */
  char typeChar();

  /* Gets the type size of the storage. */
  long typeSize();

  /**
   * Address of the off-heap storage of data.
   *
   * @return {@code 0} if there are no data to share, otherwise the address of the data in a format
   *     appropriate for this storage {@link #typeChar()} and {@link #typeSize()}.
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

  /* Gets the value at a given index as a String. */
  default String getItemAsString(long index) {
    if (isNothing(index)) {
      return null;
    }

    T item = getItemBoxed(index);
    return switch (item) {
      case LocalTime time -> time.format(DateTimeFormatter.ISO_TIME);
      case LocalDate date -> date.format(DateTimeFormatter.ISO_DATE);
      case ZonedDateTime dateTime -> dateTime.format(DateTimeFormatter.ISO_ZONED_DATE_TIME);
      case BigDecimal bigDecimal -> bigDecimal.unscaledValue() + "E" + (-bigDecimal.scale());
      default -> item.toString();
    };
  }

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
