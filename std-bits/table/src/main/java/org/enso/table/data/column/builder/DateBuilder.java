package org.enso.table.data.column.builder;

import java.time.LocalDate;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for string columns. */
public class DateBuilder extends TypedBuilderImpl<LocalDate> {
  @Override
  protected LocalDate[] newArray(int size) {
    return new LocalDate[size];
  }

  public DateBuilder(int size) {
    super(size);
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
}
