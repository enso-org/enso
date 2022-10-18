package org.enso.table.data.column.storage;

import java.time.LocalTime;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;

public final class TimeOfDayStorage extends SpecializedStorage<LocalTime> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public TimeOfDayStorage(LocalTime[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<LocalTime, SpecializedStorage<LocalTime>> ops = buildOps();

  private static MapOpStorage<LocalTime, SpecializedStorage<LocalTime>> buildOps() {
    MapOpStorage<LocalTime, SpecializedStorage<LocalTime>> t = ObjectStorage.buildObjectOps();
    t.add(SpecializedIsInOp.makeForTimeColumns(LocalTime.class));
    return t;
  }

  @Override
  protected SpecializedStorage<LocalTime> newInstance(LocalTime[] data, int size) {
    return new TimeOfDayStorage(data, size);
  }

  @Override
  protected LocalTime[] newUnderlyingArray(int size) {
    return new LocalTime[size];
  }

  @Override
  public int getType() {
    return Type.TIME_OF_DAY;
  }
}
