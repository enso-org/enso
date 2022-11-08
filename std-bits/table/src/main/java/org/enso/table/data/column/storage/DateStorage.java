package org.enso.table.data.column.storage;

import java.time.LocalDate;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;

public final class DateStorage extends SpecializedStorage<LocalDate> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateStorage(LocalDate[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<LocalDate, SpecializedStorage<LocalDate>> ops = buildOps();

  private static MapOpStorage<LocalDate, SpecializedStorage<LocalDate>> buildOps() {
    MapOpStorage<LocalDate, SpecializedStorage<LocalDate>> t = ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(LocalDate.class));
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
