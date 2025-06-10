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

  @Override
  protected SpecializedStorage<LocalDate> newInstance(LocalDate[] data) {
    return new DateStorage(data);
  }

  @Override
  protected LocalDate[] newUnderlyingArray(int size) {
    return new LocalDate[size];
  }
}
