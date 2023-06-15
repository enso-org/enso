package org.enso.table.data.column.builder.object;

import java.time.LocalDate;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;

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
    data[currentSize++] = (LocalDate) o;
  }

  public void appendDate(LocalDate date) {
    append(date);
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof LocalDate;
  }

  @Override
  public Storage<LocalDate> seal() {
    return new DateStorage(data, currentSize);
  }
}
