package org.enso.table.data.column.operation;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.function.BiFunction;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.NumericType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.error.UnexpectedTypeException;
import org.enso.table.problems.ProblemAggregator;

public class BinaryCoalescingOperation<T> extends BinaryOperationBase<T, T> {
  private static Column applyOperation(
      Column left,
      Object right,
      BiFunction<Object, Object, Object> fallback,
      StorageType<?> fallbackType,
      String name,
      MapOperationProblemAggregator problemBuilder,
      BinaryOperationTyped<?> operation,
      ColumnStorage<?> leftStorage) {
    if (right instanceof Column rightColumn) {
      if (operation != null) {
        var rightStorage = rightColumn.getStorage();
        if (!operation.canApplyZip(leftStorage, rightStorage)) {
          throw new UnexpectedTypeException(
              "Unsupported right column type: " + rightStorage.getType());
        }
        return operation.apply(left, rightColumn, name, problemBuilder);
      } else {
        // Null on left-hand side so just return the right-hand Column
        if (leftStorage.getType() instanceof NullType) {
          return new Column(name, rightColumn.getStorage());
        }

        return BinaryOperation.mapFunction(
            left, rightColumn, false, name, fallback, fallbackType, problemBuilder);
      }
    }

    if (operation != null) {
      if (!operation.canApplyMap(leftStorage, right)) {
        throw new UnexpectedTypeException("Unsupported right value type: " + right.getClass());
      }
      return operation.apply(left, right, name, problemBuilder);
    } else {
      // Null on left-hand side so just return the right-hand Column
      if (leftStorage.getType() instanceof NullType) {
        int checkedSize = Builder.checkSize(leftStorage.getSize());
        var constantStorage = Builder.fromRepeatedItem(right, checkedSize);
        return new Column(name, constantStorage);
      }

      return BinaryOperation.mapFunction(
          left, right, false, name, fallback, fallbackType, problemBuilder);
    }
  }

  private static final BinaryOperationTyped<LocalDate> DATE_MIN =
      new BinaryCoalescingOperation<>(DateType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);
  private static final BinaryOperationTyped<ZonedDateTime> DATE_TIME_MIN =
      new BinaryCoalescingOperation<>(DateTimeType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);
  private static final BinaryOperationTyped<LocalTime> TIME_MIN =
      new BinaryCoalescingOperation<>(TimeOfDayType.INSTANCE, (a, b) -> a.isBefore(b) ? a : b);
  private static final BinaryOperationTyped<String> TEXT_MIN =
      new BinaryCoalescingOperation<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) < 0 ? a : b) {
        @Override
        protected BuilderForType<String> makeStorageBuilder(
            long size,
            StorageType<?> leftType,
            StorageType<?> rightType,
            ProblemAggregator problemAggregator) {
          return TextType.maxType(leftType, rightType).makeBuilder(size, problemAggregator);
        }
      };

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
          case TextType t -> TEXT_MIN;
          case BooleanType b -> BinaryCoalescingOperationBool.MIN_INSTANCE;
          case NumericType n ->
              BinaryCoalescingOperationNumeric.create(
                  leftStorage.getType(), right, BinaryCoalescingOperationNumeric.MIN_OPERATION);
          default -> null;
        };
    return applyOperation(
        left, right, fallback, fallbackType, name, problemBuilder, operation, leftStorage);
  }

  private static final BinaryOperationTyped<LocalDate> DATE_MAX =
      new BinaryCoalescingOperation<>(DateType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);
  private static final BinaryOperationTyped<ZonedDateTime> DATE_TIME_MAX =
      new BinaryCoalescingOperation<>(DateTimeType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);
  private static final BinaryOperationTyped<LocalTime> TIME_MAX =
      new BinaryCoalescingOperation<>(TimeOfDayType.INSTANCE, (a, b) -> a.isAfter(b) ? a : b);
  private static final BinaryOperationTyped<String> TEXT_MAX =
      new BinaryCoalescingOperation<>(
          TextType.VARIABLE_LENGTH, (a, b) -> Text_Utils.compare_normalized(a, b) > 0 ? a : b) {
        @Override
        protected BuilderForType<String> makeStorageBuilder(
            long size,
            StorageType<?> leftType,
            StorageType<?> rightType,
            ProblemAggregator problemAggregator) {
          return TextType.maxType(leftType, rightType).makeBuilder(size, problemAggregator);
        }
      };

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
          case TextType t -> TEXT_MAX;
          case BooleanType b -> BinaryCoalescingOperationBool.MAX_INSTANCE;
          case NumericType n ->
              BinaryCoalescingOperationNumeric.create(
                  leftStorage.getType(), right, BinaryCoalescingOperationNumeric.MAX_OPERATION);
          default -> null;
        };
    return applyOperation(
        left, right, fallback, fallbackType, name, problemBuilder, operation, leftStorage);
  }

  private final BiFunction<T, T, T> zipOperation;

  protected BinaryCoalescingOperation(StorageType<T> validType, BiFunction<T, T, T> zipOperation) {
    super(validType, validType, false);
    this.zipOperation = zipOperation;
  }

  @Override
  public ColumnStorage<T> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
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
        makeStorageBuilder(left.getSize(), left.getType(), null, problemAggregator),
        (idx, value) -> zipOperation.apply(value, rightValueTyped));
  }

  @Override
  public ColumnStorage<T> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (NullType.INSTANCE.isOfType(right.getType())) {
      return validType.asTypedStorage(left);
    }

    return StorageIterators.zipOverStorages(
        validType.asTypedStorage(left),
        validType.asTypedStorage(right),
        size -> makeStorageBuilder(size, left.getType(), right.getType(), problemAggregator),
        false,
        (index, l, r) -> l == null ? r : (r == null ? l : zipOperation.apply(l, r)));
  }
}
