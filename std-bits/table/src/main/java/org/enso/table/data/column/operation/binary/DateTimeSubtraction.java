package org.enso.table.data.column.operation.binary;

import java.time.Duration;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationBase;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.error.UnexpectedTypeException;

public class DateTimeSubtraction<T extends Temporal> extends BinaryOperationBase<T, Object> {
  public static final DateTimeSubtraction<ZonedDateTime> DATE_TIME =
      new DateTimeSubtraction<>(DateTimeType.INSTANCE);
  public static final DateTimeSubtraction<LocalTime> TIME_OF_DAY =
      new DateTimeSubtraction<>(TimeOfDayType.INSTANCE);

  private DateTimeSubtraction(StorageType<T> validType) {
    super(validType, AnyObjectType.INSTANCE, true);
  }

  @Override
  protected ColumnStorage<Object> applyNullMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    var size = Builder.checkSize(left.getSize());
    return AnyObjectType.INSTANCE.makeBuilder(size, problemAggregator).appendNulls(size).seal();
  }

  @Override
  protected ColumnStorage<Object> applyTypedMap(
      ColumnStorage<T> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    if (rightValue == null) {
      return applyNullMap(left, rightValue, problemAggregator);
    }

    var typedRightValue = validType.valueAsType(rightValue);
    if (typedRightValue == null) {
      throw new UnexpectedTypeException("a DateTime");
    }

    return StorageIterators.mapOverStorage(
        left,
        Builder.getForAnyObject(left.getSize()),
        (index, value) -> Duration.between(typedRightValue, value));
  }

  @Override
  protected ColumnStorage<Object> applyTypedZip(
      ColumnStorage<T> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (validType.isOfType(right.getType())) {
      return StorageIterators.zipOverStorages(
          left,
          validType.asTypedStorage(right),
          Builder::getForAnyObject,
          true,
          (index, leftValue, rightValue) -> Duration.between(rightValue, leftValue));
    }

    throw new IllegalArgumentException("Unsupported storage types.");
  }
}
