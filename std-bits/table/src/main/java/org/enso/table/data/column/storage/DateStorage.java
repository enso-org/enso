package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;

import java.time.LocalDate;

public final class DateStorage extends SpecializedStorage<LocalDate> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateStorage(LocalDate[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<SpecializedStorage<LocalDate>> ops = buildOps();

  private static MapOpStorage<SpecializedStorage<LocalDate>> buildOps() {
    MapOpStorage<SpecializedStorage<LocalDate>> t = ObjectStorage.buildObjectOps();
    t.add(SpecializedIsInOp.makeForTimeColumns(LocalDate.class));
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
  public int getType() {
    return Type.DATE;
  }
}
