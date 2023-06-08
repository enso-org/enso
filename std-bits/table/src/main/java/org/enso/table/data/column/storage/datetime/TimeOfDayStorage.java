package org.enso.table.data.column.storage.datetime;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.TimeOfDayBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

import java.time.LocalTime;

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
    t.add(new DateTimeIsInOp<>(LocalTime.class));
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
  public StorageType getType() {
    return TimeOfDayType.INSTANCE;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new TimeOfDayBuilder(capacity);
  }
}
