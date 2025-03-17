package org.enso.table.data.column.operation;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import java.util.function.BiFunction;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.error.UnexpectedTypeException;
import org.enso.table.problems.BlackholeProblemAggregator;

public class BinaryCoalescingOperation<T> implements BinaryOperation<T> {
  private static Column applyOperation(
      Column left,
      Object right,
      BiFunction<Object, Object, Object> fallback,
      StorageType<?> fallbackType,
      String name,
      MapOperationProblemAggregator problemBuilder,
      BinaryOperation<? extends Temporal> operation,
      Storage<?> leftStorage,
      String fallbackName) {
    if (right instanceof Column rightColumn) {
      if (operation != null) {
        var rightStorage = rightColumn.getStorage();
        if (!operation.canApplyZip(leftStorage, rightStorage)) {
          throw new UnexpectedTypeException(
              "Unsupported right column type: " + rightStorage.getType());
        }
        return operation.apply(left, rightColumn, name);
      } else {
        var result =
            leftStorage.vectorizedOrFallbackZip(
                fallbackName,
                problemBuilder,
                fallback,
                rightColumn.getStorage(),
                false,
                fallbackType);
        return new Column(name, result);
      }
    }

    if (operation != null) {
      if (!operation.canApplyMap(leftStorage, right)) {
        throw new UnexpectedTypeException("Unsupported right value type: " + right.getClass());
      }
      return operation.apply(left, right, name);
    } else {
      var result =
          leftStorage.vectorizedOrFallbackBinaryMap(
              fallbackName, problemBuilder, fallback, right, false, leftStorage.getType());
      return new Column(name, result);
    }
  }

  private static final BinaryOperation<LocalDate> DATE_MIN =
      new BinaryCoalescingOperation<>(DateType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);
  private static final BinaryOperation<ZonedDateTime> DATE_TIME_MIN =
      new BinaryCoalescingOperation<>(DateTimeType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);
  private static final BinaryOperation<LocalTime> TIME_MIN =
      new BinaryCoalescingOperation<>(TimeOfDayType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);

  public static Column min(
      Column left,
      Object right,
      BiFunction<Object, Object, Object> fallback,
      StorageType<?> fallbackType,
      String name,
      MapOperationProblemAggregator problemBuilder) {
    var leftStorage = left.getStorage();
    var operation =
        switch (leftStorage.getType()) {
          case DateType d -> DATE_MIN;
          case DateTimeType dt -> DATE_TIME_MIN;
          case TimeOfDayType t -> TIME_MIN;
          default -> null;
        };
    return applyOperation(
        left, right, fallback, fallbackType, name, problemBuilder, operation, leftStorage, "min");
  }

  private static final BinaryOperation<LocalDate> DATE_MAX =
      new BinaryCoalescingOperation<>(DateType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);
  private static final BinaryOperation<ZonedDateTime> DATE_TIME_MAX =
      new BinaryCoalescingOperation<>(DateTimeType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);
  private static final BinaryOperation<LocalTime> TIME_MAX =
      new BinaryCoalescingOperation<>(TimeOfDayType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);

  public static Column max(
      Column left,
      Object right,
      BiFunction<Object, Object, Object> fallback,
      StorageType<?> fallbackType,
      String name,
      MapOperationProblemAggregator problemBuilder) {
    var leftStorage = left.getStorage();
    var operation =
        switch (leftStorage.getType()) {
          case DateType d -> DATE_MAX;
          case DateTimeType dt -> DATE_TIME_MAX;
          case TimeOfDayType t -> TIME_MAX;
          default -> null;
        };
    return applyOperation(
        left, right, fallback, fallbackType, name, problemBuilder, operation, leftStorage, "max");
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
      throw new IllegalArgumentException(
          "Unsupported right value type " + rightValue.getClass() + ".");
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
