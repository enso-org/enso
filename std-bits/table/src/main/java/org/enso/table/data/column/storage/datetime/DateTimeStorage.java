package org.enso.table.data.column.storage.datetime;

import java.time.ZonedDateTime;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.DateTimeType;

public final class DateTimeStorage extends SpecializedStorage<ZonedDateTime> {
  /**
   * @param data the underlying data
   */
  public DateTimeStorage(ZonedDateTime[] data) {
    super(DateTimeType.INSTANCE, data);
  }
}
