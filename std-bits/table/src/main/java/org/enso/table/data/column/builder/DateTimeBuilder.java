package org.enso.table.data.column.builder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.BitSet;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for ZonedDateTime columns. */
final class DateTimeBuilder extends TypedBuilder<ZonedDateTime> {
  private final boolean allowDateToDateTimeConversion;
  private final BitSet wasLocalDate;

  DateTimeBuilder(int size, boolean allowDateToDateTimeConversion) {
    super(DateTimeType.INSTANCE, new ZonedDateTime[size]);
    this.allowDateToDateTimeConversion = allowDateToDateTimeConversion;
    this.wasLocalDate = allowDateToDateTimeConversion ? new BitSet(size) : null;
  }

  /**
   * TODO DRY {@link org.enso.table.data.column.operation.cast.ToDateTimeStorageConverter}
   * convertDate.
   */
  private ZonedDateTime convertDate(LocalDate date) {
    return date.atStartOfDay().atZone(ZoneId.systemDefault());
  }

  @Override
  public DateTimeBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        if (allowDateToDateTimeConversion && o instanceof LocalDate localDate) {
          data[currentSize++] = convertDate(localDate);
          wasLocalDate.set(currentSize - 1);
        } else if (o instanceof LocalDateTime localDateTime) {
          data[currentSize++] = localDateTime.atZone(ZoneId.systemDefault());
        } else {
          data[currentSize++] = (ZonedDateTime) o;
        }
      } catch (ClassCastException e) {
        throw new ValueTypeMismatchException(getType(), o);
      }
    }
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof DateType dateType) {
      var typedStorage = dateType.asTypedStorage(storage);
      long n = typedStorage.getSize();
      for (long i = 0; i < n; i++) {
        var date = typedStorage.getItemBoxed(i);
        this.append(date == null ? null : convertDate(date));
      }
    } else {
      super.appendBulkStorage(storage);
    }
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof ZonedDateTime || (allowDateToDateTimeConversion && o instanceof LocalDate);
  }

  @Override
  protected ColumnStorage<ZonedDateTime> doSeal() {
    return new TypedStorage<>(DateTimeType.INSTANCE, data);
  }

  @Override
  public void copyDataTo(Object[] items) {
    if (allowDateToDateTimeConversion) {
      if (currentSize >= 0) {
        System.arraycopy(data, 0, items, 0, currentSize);

        // Replace ZonedDateTime with LocalDate where necessary.
        int next = this.wasLocalDate.nextSetBit(0);
        while (next != -1) {
          items[next] = data[next].toLocalDate();
          next = this.wasLocalDate.nextSetBit(next + 1);
        }
      }
    } else {
      super.copyDataTo(items);
    }
  }
}
