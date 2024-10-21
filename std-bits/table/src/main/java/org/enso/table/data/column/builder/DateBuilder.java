package org.enso.table.data.column.builder;

import java.time.LocalDate;
import java.util.Objects;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for LocalDate columns. */
public class DateBuilder extends TypedBuilderImpl<LocalDate> {
  @Override
  protected LocalDate[] newArray(int size) {
    return new LocalDate[size];
  }

  private final boolean allowDateToDateTimeConversion;

  public DateBuilder(int size) {
    this(size, false);
  }

  public DateBuilder(int size, boolean allowDateToDateTimeConversion) {
    super(size);
    this.allowDateToDateTimeConversion = allowDateToDateTimeConversion;
  }

  @Override
  public StorageType getType() {
    return DateType.INSTANCE;
  }

  @Override
  public void appendNoGrow(Object o) {
    try {
      data[currentSize++] = (LocalDate) o;
    } catch (ClassCastException e) {
      throw new ValueTypeMismatchException(getType(), o);
    }
  }

  public void appendDate(LocalDate date) {
    append(date);
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof LocalDate;
  }

  @Override
  protected Storage<LocalDate> doSeal() {
    return new DateStorage(data, currentSize);
  }

  @Override
  public boolean canRetypeTo(StorageType type) {
    if (allowDateToDateTimeConversion && Objects.equals(type, DateTimeType.INSTANCE)) {
      return true;
    }
    return super.canRetypeTo(type);
  }

  @Override
  public TypedBuilder retypeTo(StorageType type) {
    if (allowDateToDateTimeConversion && Objects.equals(type, DateTimeType.INSTANCE)) {
      DateTimeBuilder res = new DateTimeBuilder(data.length, true);
      for (int i = 0; i < currentSize; i++) {
        res.appendNoGrow(data[i]);
      }
      return res;
    }
    return super.retypeTo(type);
  }
}
