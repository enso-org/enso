package org.enso.table.data.column.storage.datetime;

import java.time.Duration;
import java.time.LocalTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.ObjectBuilder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.datetime.DatePartExtractors;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

public final class TimeOfDayStorage extends SpecializedStorage<LocalTime> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public TimeOfDayStorage(LocalTime[] data, int size) {
    super(data, size, buildOps());
  }

  private static MapOperationStorage<LocalTime, SpecializedStorage<LocalTime>> buildOps() {
    MapOperationStorage<LocalTime, SpecializedStorage<LocalTime>> t =
        ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(LocalTime.class));
    t.add(DatePartExtractors.hour());
    t.add(DatePartExtractors.minute());
    t.add(DatePartExtractors.second());
    t.add(DatePartExtractors.millisecond());
    t.add(DatePartExtractors.microsecond());
    t.add(DatePartExtractors.nanosecond());
    t.add(
        new GenericBinaryObjectMapOperation<LocalTime, SpecializedStorage<LocalTime>, Duration>(
            Maps.SUB, LocalTime.class, TimeOfDayStorage.class) {
          @Override
          protected Builder createOutputBuilder(int size) {
            return new ObjectBuilder(size);
          }

          @Override
          protected Duration run(LocalTime value, LocalTime other) {
            return Duration.between(other, value);
          }
        });
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
}
