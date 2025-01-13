package org.enso.table.data.column.storage.datetime;

import java.time.Duration;
import java.time.LocalTime;
import org.enso.base.CompareException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.ObjectBuilder;
import org.enso.table.data.column.builder.TimeOfDayBuilder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.operation.map.datetime.TimeLikeBinaryOpReturningBoolean;
import org.enso.table.data.column.operation.map.datetime.TimeLikeCoalescingOperation;
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
    t.add(
        new TimeLikeBinaryOpReturningBoolean<>(Maps.EQ, LocalTime.class) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return a.equals(b);
          }

          @Override
          protected boolean doOther(LocalTime a, Object b) {
            return false;
          }
        });
    t.add(
        new TimeOfDayComparisonOp(Maps.LT) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return a.compareTo(b) < 0;
          }
        });
    t.add(
        new TimeOfDayComparisonOp(Maps.LTE) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return a.compareTo(b) <= 0;
          }
        });
    t.add(
        new TimeOfDayComparisonOp(Maps.GT) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return a.compareTo(b) > 0;
          }
        });
    t.add(
        new TimeOfDayComparisonOp(Maps.GTE) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return a.compareTo(b) >= 0;
          }
        });
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
    t.add(
        new TimeLikeCoalescingOperation<>(Maps.MIN, LocalTime.class) {
          @Override
          protected Builder createOutputBuilder(int size) {
            return new TimeOfDayBuilder(size);
          }

          @Override
          protected LocalTime doOperation(LocalTime a, LocalTime b) {
            return a.compareTo(b) < 0 ? a : b;
          }
        });
    t.add(
        new TimeLikeCoalescingOperation<>(Maps.MAX, LocalTime.class) {
          @Override
          protected Builder createOutputBuilder(int size) {
            return new TimeOfDayBuilder(size);
          }

          @Override
          protected LocalTime doOperation(LocalTime a, LocalTime b) {
            return a.compareTo(b) > 0 ? a : b;
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

  private abstract static class TimeOfDayComparisonOp
      extends TimeLikeBinaryOpReturningBoolean<LocalTime> {
    public TimeOfDayComparisonOp(String name) {
      super(name, LocalTime.class);
    }

    @Override
    protected boolean doOther(LocalTime a, Object b) {
      throw new CompareException(a, b);
    }
  }
}
