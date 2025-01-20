package org.enso.table.data.column.builder;

import java.time.LocalTime;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.TimeOfDayStorage;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for LocalTime columns. */
public class TimeOfDayBuilder extends TypedBuilder<LocalTime> {
  TimeOfDayBuilder(int size) {
    super(TimeOfDayType.INSTANCE, new LocalTime[size]);
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
