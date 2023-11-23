package org.enso.table.data.column.storage.datetime;

import java.time.LocalDate;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.datetime.DatePartExtractors;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;

public final class DateStorage extends SpecializedStorage<LocalDate> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateStorage(LocalDate[] data, int size) {
    super(data, size, buildOps());
  }

  private static MapOperationStorage<LocalDate, SpecializedStorage<LocalDate>> buildOps() {
    MapOperationStorage<LocalDate, SpecializedStorage<LocalDate>> t =
        ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(LocalDate.class));
    t.add(DatePartExtractors.year());
    t.add(DatePartExtractors.quarter());
    t.add(DatePartExtractors.month());
    t.add(DatePartExtractors.week());
    t.add(DatePartExtractors.day());
    return t;
  }

  @Override
  protected SpecializedStorage<LocalDate> newInstance(LocalDate[] data, int size) {
    return new DateStorage(data, size);
  }

  @Override
  protected LocalDate[] newUnderlyingArray(int size) {
    return new LocalDate[size];
  }

  @Override
  public StorageType getType() {
    return DateType.INSTANCE;
  }
}
