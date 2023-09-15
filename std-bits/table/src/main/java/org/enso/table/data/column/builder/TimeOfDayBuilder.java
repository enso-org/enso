package org.enso.table.data.column.builder;

import java.time.LocalTime;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for string columns. */
public class TimeOfDayBuilder extends TypedBuilderImpl<LocalTime> {
  @Override
  protected LocalTime[] newArray(int size) {
    return new LocalTime[size];
  }

  public TimeOfDayBuilder(int size) {
    super(size);
  }

  @Override
  public StorageType getType() {
    return TimeOfDayType.INSTANCE;
  }

  @Override
  public void appendNoGrow(Object o) {
    try {
      data[currentSize++] = (LocalTime) o;
    } catch (ClassCastException e) {
      throw new ValueTypeMismatchException(getType(), o);
    }
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof LocalTime;
  }

  @Override
  protected Storage<LocalTime> doSeal() {
    return new TimeOfDayStorage(data, currentSize);
  }
}
