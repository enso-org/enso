package org.enso.table.data.column.storage.datetime;

import java.time.Duration;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.ObjectBuilder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.bool.GenericBinaryOpReturningBoolean;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.StorageType;

public final class DateTimeStorage extends SpecializedStorage<ZonedDateTime> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateTimeStorage(ZonedDateTime[] data, int size) {
    super(data, size, buildOps());
  }

  private static MapOperationStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> buildOps() {
    MapOperationStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> t =
        ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(ZonedDateTime.class));
    t.add(
        new GenericBinaryOpReturningBoolean<>(Maps.EQ, ZonedDateTime.class) {
          @Override
          protected boolean doOperation(ZonedDateTime a, ZonedDateTime b) {
            return a.isEqual(b);
          }
        });
    t.add(
        new GenericBinaryOpReturningBoolean<>(Maps.LT, ZonedDateTime.class) {
          @Override
          protected boolean doOperation(ZonedDateTime a, ZonedDateTime b) {
            return a.compareTo(b) < 0;
          }
        });
    t.add(
        new GenericBinaryOpReturningBoolean<>(Maps.LTE, ZonedDateTime.class) {
          @Override
          protected boolean doOperation(ZonedDateTime a, ZonedDateTime b) {
            return a.compareTo(b) <= 0;
          }
        });
    t.add(
        new GenericBinaryOpReturningBoolean<>(Maps.GT, ZonedDateTime.class) {
          @Override
          protected boolean doOperation(ZonedDateTime a, ZonedDateTime b) {
            return a.compareTo(b) > 0;
          }
        });
    t.add(
        new GenericBinaryOpReturningBoolean<>(Maps.GTE, ZonedDateTime.class) {
          @Override
          protected boolean doOperation(ZonedDateTime a, ZonedDateTime b) {
            return a.compareTo(b) >= 0;
          }
        });
    t.add(
        new GenericBinaryObjectMapOperation<
            ZonedDateTime, SpecializedStorage<ZonedDateTime>, Duration>(
            Maps.SUB, ZonedDateTime.class, DateTimeStorage.class) {
          @Override
          protected Builder createOutputBuilder(int size) {
            return new ObjectBuilder(size);
          }

          @Override
          protected Duration run(ZonedDateTime value, ZonedDateTime other) {
            return Duration.between(other, value);
          }
        });
    return t;
  }

  @Override
  protected SpecializedStorage<ZonedDateTime> newInstance(ZonedDateTime[] data, int size) {
    return new DateTimeStorage(data, size);
  }

  @Override
  protected ZonedDateTime[] newUnderlyingArray(int size) {
    return new ZonedDateTime[size];
  }

  @Override
  public StorageType getType() {
    return DateTimeType.INSTANCE;
  }
}
