package org.enso.table.data.column.storage.datetime;

import java.time.LocalTime;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.TimeOfDayType;

public final class TimeOfDayStorage extends SpecializedStorage<LocalTime> {
  /**
   * @param data the underlying data
   */
  public TimeOfDayStorage(LocalTime[] data) {
    super(TimeOfDayType.INSTANCE, data);
  }
}
