package org.enso.table.data.column.builder;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.BitSet;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.error.ValueTypeMismatchException;
import org.graalvm.polyglot.Context;

/** A builder for ZonedDateTime columns. */
public final class DateTimeBuilder extends TypedBuilder<ZonedDateTime> {
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
  public void append(Object o) {
    ensureSpaceToAppend();
    try {
      if (allowDateToDateTimeConversion && o instanceof LocalDate localDate) {
        data[currentSize++] = convertDate(localDate);
        wasLocalDate.set(currentSize - 1);
      } else {
        data[currentSize++] = (ZonedDateTime) o;
      }
    } catch (ClassCastException e) {
      throw new ValueTypeMismatchException(getType(), o);
    }
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    if (storage.getType() instanceof DateType) {
      Context context = Context.getCurrent();
      for (long i = 0; i < storage.getSize(); ++i) {
        var date = storage.getItemBoxed(i);
        if (date == null) {
          appendNulls(1);
        } else if (date instanceof LocalDate localDate) {
          append(convertDate(localDate));
        } else {
          throw new IllegalStateException("Unexpected type in DateStorage: " + date.getClass());
        }
        context.safepoint();
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
  protected Storage<ZonedDateTime> doSeal() {
    return new DateTimeStorage(data);
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
