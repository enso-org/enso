package org.enso.table.data.column.storage;

import java.time.LocalTime;
import java.time.ZonedDateTime;

import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.builder.object.TimeOfDayBuilder;
import org.enso.table.data.column.operation.CastProblemBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

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

  @Override
  public Storage<?> cast(StorageType targetType, CastProblemBuilder castProblemBuilder) {
    if (targetType instanceof TextType textType) {
      int n = size();
      StringBuilder builder = new StringBuilder(n);
      var formatter = Core_Date_Utils.defaultLocalTimeFormatter();
      for (int i = 0; i < n; i++) {
        LocalTime item = data[i];
        if (item == null) {
          builder.appendNulls(1);
        } else {
          builder.append(item.format(formatter));
        }
      }
      return StringStorage.adapt(builder.seal(), textType);
    } else {
      return super.cast(targetType, castProblemBuilder);
    }
  }
}
