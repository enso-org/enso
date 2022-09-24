package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.map.MapOpStorage;

import java.time.LocalTime;

public class TimeOfDayStorage extends SpecializedStorage<LocalTime> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public TimeOfDayStorage(LocalTime[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<SpecializedStorage<LocalTime>> ops = buildOps();

  private static MapOpStorage<SpecializedStorage<LocalTime>> buildOps() {
    return ObjectStorage.buildObjectOps();
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
