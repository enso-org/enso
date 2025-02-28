package org.enso.table.data.column.operation;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.function.BiFunction;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;
import org.enso.table.problems.BlackholeProblemAggregator;

public class BinaryCoalescingOperation<T> implements BinaryOperation<T> {
  private static final BinaryOperation<LocalDate> DATE_MIN =
      new BinaryCoalescingOperation<>(DateType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);
  private static final BinaryOperation<ZonedDateTime> DATE_TIME_MIN =
      new BinaryCoalescingOperation<>(DateTimeType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);
  private static final BinaryOperation<LocalTime> TIME_MIN =
      new BinaryCoalescingOperation<>(TimeOfDayType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);

  public static BinaryOperation<?> min(Column left) {
    var leftStorage = left.getStorage();
    return switch (leftStorage.getType()) {
      case DateType d -> DATE_MIN;
      case DateTimeType dt -> DATE_TIME_MIN;
      case TimeOfDayType t -> TIME_MIN;
      default -> null;
    };
  }

  private static final BinaryOperation<LocalDate> DATE_MAX =
      new BinaryCoalescingOperation<>(DateType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);
  private static final BinaryOperation<ZonedDateTime> DATE_TIME_MAX =
      new BinaryCoalescingOperation<>(DateTimeType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);
  private static final BinaryOperation<LocalTime> TIME_MAX =
      new BinaryCoalescingOperation<>(TimeOfDayType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);

  public static BinaryOperation<?> max(Column left) {
    var leftStorage = left.getStorage();
    return switch (leftStorage.getType()) {
      case DateType d -> DATE_MAX;
      case DateTimeType dt -> DATE_TIME_MAX;
      case TimeOfDayType t -> TIME_MAX;
      default -> null;
    };
  }

  private final StorageType<T> validType;
  private final BiFunction<T, T, T> zipOperation;

  private BinaryCoalescingOperation(StorageType<T> validType, BiFunction<T, T, T> zipOperation) {
    this.validType = validType;
    this.zipOperation = zipOperation;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return validType.isOfType(left.getType());
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return canApplyMap(left, null)
        && (NullType.INSTANCE.isOfType(right.getType()) || canApplyMap(right, null));
  }

  @Override
  public ColumnStorage<T> applyMap(ColumnStorage<?> left, Object rightValue) {
    if (rightValue == null) {
      return validType.asTypedStorage(left);
    }

    T rightValueTyped = validType.valueAsType(rightValue);
    if (rightValueTyped == null) {
      throw new IllegalArgumentException("Unsupported right value type.");
    }

    return StorageIterators.mapOverStorage(
        validType.asTypedStorage(left),
        false,
        validType.makeBuilder(left.getSize(), BlackholeProblemAggregator.INSTANCE),
        (idx, value) -> zipOperation.apply(value, rightValueTyped));
  }

  @Override
  public ColumnStorage<T> applyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    if (NullType.INSTANCE.isOfType(right.getType())) {
      return validType.asTypedStorage(left);
    }

    return StorageIterators.zipOverStorages(
        validType.asTypedStorage(left),
        validType.asTypedStorage(right),
        size -> validType.makeBuilder(size, BlackholeProblemAggregator.INSTANCE),
        false,
        (index, l, r) -> l == null ? r : (r == null ? l : zipOperation.apply(l, r)));
  }
}
