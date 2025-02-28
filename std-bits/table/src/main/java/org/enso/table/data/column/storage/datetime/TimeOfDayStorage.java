package org.enso.table.data.column.storage.datetime;

import java.time.Duration;
import java.time.LocalTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.TimeOfDayType;

public final class TimeOfDayStorage extends SpecializedStorage<LocalTime> {
  /**
   * @param data the underlying data
   */
  public TimeOfDayStorage(LocalTime[] data) {
    super(TimeOfDayType.INSTANCE, data, buildOps());
  }

  private static MapOperationStorage<LocalTime, SpecializedStorage<LocalTime>> buildOps() {
    MapOperationStorage<LocalTime, SpecializedStorage<LocalTime>> t = new MapOperationStorage<>();
    t.add(new DateTimeIsInOp<>(LocalTime.class));
    t.add(
        new GenericBinaryObjectMapOperation<LocalTime, SpecializedStorage<LocalTime>, Duration>(
            Maps.SUB, LocalTime.class, TimeOfDayStorage.class) {
          @Override
          protected Builder createOutputBuilder(long size) {
            return Builder.getObjectBuilder(size);
          }

          @Override
          protected Duration run(LocalTime value, LocalTime other) {
            return Duration.between(other, value);
          }
        });
    return t;
  }

  @Override
  protected SpecializedStorage<LocalTime> newInstance(LocalTime[] data) {
    return new TimeOfDayStorage(data);
  }

  @Override
  protected LocalTime[] newUnderlyingArray(int size) {
    return new LocalTime[size];
  }
}
