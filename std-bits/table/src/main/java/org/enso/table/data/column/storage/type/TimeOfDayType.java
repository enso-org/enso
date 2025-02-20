package org.enso.table.data.column.storage.type;

import java.time.LocalTime;
import org.enso.table.data.column.storage.ColumnStorage;

public record TimeOfDayType() implements StorageType {
  public static final TimeOfDayType INSTANCE = new TimeOfDayType();

  @Override
  public boolean isNumeric() {
    return false;
  }

  @Override
  public boolean hasDate() {
    return false;
  }

  @Override
  public boolean hasTime() {
    return true;
  }

  public ColumnStorage<LocalTime> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof TimeOfDayType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<LocalTime>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of TimeOfDayType");
  }
}
