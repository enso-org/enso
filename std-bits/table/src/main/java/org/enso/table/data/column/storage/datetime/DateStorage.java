package org.enso.table.data.column.storage.datetime;

import java.time.LocalDate;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.DateType;

public final class DateStorage extends SpecializedStorage<LocalDate> {
  /**
   * @param data the underlying data
   */
  public DateStorage(LocalDate[] data) {
    super(DateType.INSTANCE, data);
  }
}
