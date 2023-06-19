package org.enso.table.data.column.builder.object;

import java.time.LocalTime;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

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
    data[currentSize++] = (LocalTime) o;
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof LocalTime;
  }

  @Override
  public Storage<LocalTime> seal() {
    return new TimeOfDayStorage(data, currentSize);
  }
}
