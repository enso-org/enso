package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.map.MapOpStorage;

import java.time.LocalDate;

public class DateStorage extends SpecializedStorage<LocalDate> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateStorage(LocalDate[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<SpecializedStorage<LocalDate>> ops = buildOps();

  private static MapOpStorage<SpecializedStorage<LocalDate>> buildOps() {
    return ObjectStorage.buildObjectOps();
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
  public int getType() {
    return Type.DATE;
  }
}
