package org.enso.table.data.column.storage.datetime;

import java.time.Duration;
import java.time.LocalTime;
import org.enso.base.CompareException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;
import org.enso.table.data.column.operation.map.datetime.TimeLikeBinaryOpReturningBoolean;
import org.enso.table.data.column.operation.map.datetime.TimeLikeCoalescingOperation;
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
            return a.isBefore(b);
          }
        });
    t.add(
        new TimeOfDayComparisonOp(Maps.LTE) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return !a.isAfter(b);
          }
        });
    t.add(
        new TimeOfDayComparisonOp(Maps.GT) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return a.isAfter(b);
          }
        });
    t.add(
        new TimeOfDayComparisonOp(Maps.GTE) {
          @Override
          protected boolean doOperation(LocalTime a, LocalTime b) {
            return !a.isBefore(b);
          }
        });
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
    t.add(
        new TimeLikeCoalescingOperation<>(Maps.MIN, LocalTime.class) {
          @Override
          protected Builder createOutputBuilder(long size) {
            return Builder.getForTime(size);
          }

          @Override
          protected LocalTime doOperation(LocalTime a, LocalTime b) {
            return a.isBefore(b) ? a : b;
          }
        });
    t.add(
        new TimeLikeCoalescingOperation<>(Maps.MAX, LocalTime.class) {
          @Override
          protected Builder createOutputBuilder(long size) {
            return Builder.getForTime(size);
          }

          @Override
          protected LocalTime doOperation(LocalTime a, LocalTime b) {
            return a.isAfter(b) ? a : b;
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
