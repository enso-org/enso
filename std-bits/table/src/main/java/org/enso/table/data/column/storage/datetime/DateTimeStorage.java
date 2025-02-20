package org.enso.table.data.column.storage.datetime;

import java.time.Duration;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.operation.map.datetime.TimeLikeCoalescingOperation;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.DateTimeType;

public final class DateTimeStorage extends SpecializedStorage<ZonedDateTime> {
  /**
   * @param data the underlying data
   */
  public DateTimeStorage(ZonedDateTime[] data) {
    super(DateTimeType.INSTANCE, data, buildOps());
  }

  private static MapOperationStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> buildOps() {
    MapOperationStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> t =
        new MapOperationStorage<>();
    t.add(new DateTimeIsInOp<>(ZonedDateTime.class));
    t.add(
        new GenericBinaryObjectMapOperation<
            ZonedDateTime, SpecializedStorage<ZonedDateTime>, Duration>(
            Maps.SUB, ZonedDateTime.class, DateTimeStorage.class) {
          @Override
          protected Builder createOutputBuilder(long size) {
            return Builder.getObjectBuilder(size);
          }

          @Override
          protected Duration run(ZonedDateTime value, ZonedDateTime other) {
            return Duration.between(other, value);
          }
        });
    t.add(
        new TimeLikeCoalescingOperation<>(Maps.MIN, ZonedDateTime.class) {
          @Override
          protected Builder createOutputBuilder(long size) {
            return Builder.getForDateTime(size);
          }

          @Override
          protected ZonedDateTime doOperation(ZonedDateTime a, ZonedDateTime b) {
            return a.compareTo(b) < 0 ? a : b;
          }
        });
    t.add(
        new TimeLikeCoalescingOperation<>(Maps.MAX, ZonedDateTime.class) {
          @Override
          protected Builder createOutputBuilder(long size) {
            return Builder.getForDateTime(size);
          }

          @Override
          protected ZonedDateTime doOperation(ZonedDateTime a, ZonedDateTime b) {
            return a.compareTo(b) > 0 ? a : b;
          }
        });
    return t;
  }

  @Override
  protected SpecializedStorage<ZonedDateTime> newInstance(ZonedDateTime[] data) {
    return new DateTimeStorage(data);
  }

  @Override
  protected ZonedDateTime[] newUnderlyingArray(int size) {
    return new ZonedDateTime[size];
  }
}
