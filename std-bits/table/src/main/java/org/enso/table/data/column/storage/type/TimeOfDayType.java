package org.enso.table.data.column.storage.type;

import org.enso.table.data.column.storage.ColumnStorage;

import java.time.LocalTime;

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
      //noinspection unchecked
      return (ColumnStorage<LocalTime>) storage;
    }
    throw new IllegalArgumentException("Storage is not of TimeOfDayType");
  }
}
