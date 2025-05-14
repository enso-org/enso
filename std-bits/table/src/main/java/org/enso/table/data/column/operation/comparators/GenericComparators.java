package org.enso.table.data.column.operation.comparators;

import java.util.function.BiPredicate;
import org.enso.base.CompareException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;

public abstract class GenericComparators<T> implements BinaryOperation<Boolean> {
  protected final BiPredicate<T, T> comparator;
  protected final boolean throwOnOther;

  protected GenericComparators(BiPredicate<T, T> comparator, boolean throwOnOther) {
    this.comparator = comparator;
    this.throwOnOther = throwOnOther;
  }

  protected abstract T asTypedValue(Object value);

  protected abstract ColumnStorage<T> asTypedStorage(ColumnStorage<?> storage);

  protected RuntimeException makeCompareError(Object left, Object right) {
    return new CompareException(left, right);
  }

  @Override
  public abstract boolean canApplyMap(ColumnStorage<?> left, Object rightValue);

  @Override
  public ColumnStorage<Boolean> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    if (left.getType() instanceof NullType || rightValue == null) {
      return BoolStorage.makeEmpty(left.getSize());
    }

    assert canApplyMap(left, rightValue);

    var builder = Builder.getForBoolean(left.getSize());
    var typedLeft = asTypedStorage(left);
    var typedRight = asTypedValue(rightValue);

    if (typedRight != null) {
      return StorageIterators.buildOverStorage(
          typedLeft,
          builder,
          (b, index, value) -> b.appendBoolean(comparator.test(value, typedRight)));
    } else if (throwOnOther) {
      // If all are Nothing then will return a Nothing Boolean Storage
      return StorageIterators.buildOverStorage(
          typedLeft,
          builder,
          (b, index, value) -> {
            throw makeCompareError(value, rightValue);
          });
    } else {
      return StorageIterators.buildOverStorage(
          typedLeft, builder, (b, index, value) -> b.appendBoolean(false));
    }
  }

  @Override
  public abstract boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right);

  @Override
  public ColumnStorage<Boolean> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (left.getType() instanceof NullType || right.getType() instanceof NullType) {
      var size = Math.max(left.getSize(), right.getSize());
      return BoolStorage.makeEmpty(size);
    }

    assert canApplyZip(left, right);

    var typedLeft = asTypedStorage(left);
    if (right.getType() instanceof AnyObjectType) {
      // Fall back to iterating over each.
      return StorageIterators.zipOverStorages(
          typedLeft,
          right,
          Builder::getForBoolean,
          true,
          (index, leftValue, rightValue) -> {
            T typedRightValue = asTypedValue(rightValue);
            if (typedRightValue == null) {
              if (throwOnOther) {
                throw makeCompareError(leftValue, rightValue);
              } else {
                return false;
              }
            } else {
              return comparator.test(leftValue, typedRightValue);
            }
          });
    }

    // Use it as a typed storage.
    var typedRight = asTypedStorage(right);
    return StorageIterators.zipOverStorages(
        typedLeft,
        typedRight,
        Builder::getForBoolean,
        true,
        (index, leftValue, rightValue) -> comparator.test(leftValue, rightValue));
  }
}
