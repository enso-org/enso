package org.enso.table.data.column.storage.type;

import java.time.ZonedDateTime;
import org.enso.table.data.column.storage.ColumnStorage;

public record DateTimeType() implements StorageType<ZonedDateTime> {
  public static final DateTimeType INSTANCE = new DateTimeType();

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
    return true;
  }

  @Override
  public ColumnStorage<ZonedDateTime> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof DateTimeType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<ZonedDateTime>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of DateTimeType");
  }
}
