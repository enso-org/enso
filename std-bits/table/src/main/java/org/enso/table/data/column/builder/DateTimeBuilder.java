package org.enso.table.data.column.builder;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.BitSet;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;
import org.graalvm.polyglot.Context;

/** A builder for ZonedDateTime columns. */
public class DateTimeBuilder extends TypedBuilderImpl<ZonedDateTime> {
  @Override
  protected ZonedDateTime[] newArray(int size) {
    return new ZonedDateTime[size];
  }

  private final boolean allowDateToDateTimeConversion;
  private final BitSet wasLocalDate;

  public DateTimeBuilder(int size) {
    this(size, false);
  }

  public DateTimeBuilder(int size, boolean allowDateToDateTimeConversion) {
    super(size);
    this.allowDateToDateTimeConversion = allowDateToDateTimeConversion;
    this.wasLocalDate = allowDateToDateTimeConversion ? new BitSet(size) : null;
  }

  @Override
  public StorageType getType() {
    return DateTimeType.INSTANCE;
  }

  /**
   * TODO DRY {@link org.enso.table.data.column.operation.cast.ToDateTimeStorageConverter}
   * convertDate.
   */
  private ZonedDateTime convertDate(LocalDate date) {
    return date.atStartOfDay().atZone(ZoneId.systemDefault());
  }

  @Override
  public void appendNoGrow(Object o) {
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
      if (storage instanceof DateStorage dateStorage) {
        Context context = Context.getCurrent();
        for (int i = 0; i < dateStorage.size(); ++i) {
          LocalDate date = dateStorage.getItemBoxed(i);
          if (date == null) {
            data[currentSize++] = null;
          } else {
            data[currentSize++] = convertDate(date);
          }

          context.safepoint();
        }
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type "
                + storage.getType()
                + ": "
                + storage
                + ". This is a bug in the Table library.");
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
    return new DateTimeStorage(data, currentSize);
  }

  @Override
  public void retypeToMixed(Object[] items) {
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
      super.retypeToMixed(items);
    }
  }
}
