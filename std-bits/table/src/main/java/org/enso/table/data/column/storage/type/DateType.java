package org.enso.table.data.column.storage.type;

import java.time.LocalDate;
import org.enso.table.data.column.storage.ColumnStorage;

public record DateType() implements StorageType<LocalDate> {
  public static final DateType INSTANCE = new DateType();

  @Override
  public boolean isNumeric() {
    return false;
  }

  @Override
  public boolean hasDate() {
    return true;
  }

  @Override
  public boolean hasTime() {
    return false;
  }

  @Override
  public ColumnStorage<LocalDate> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof DateType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<LocalDate>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of DateType");
  }
}
