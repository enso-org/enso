package org.enso.table.data.column.operation.comparators;

import java.util.function.BiPredicate;
import org.enso.base.CompareException;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public class GenericComparators<T> implements BinaryOperationTyped<Boolean> {
  protected final StorageType<T> valueType;
  protected final BiPredicate<T, T> comparator;
  protected final boolean throwOnOther;
  protected final boolean valueOnOther;

  public GenericComparators(StorageType<T> valueType, BiPredicate<T, T> comparator) {
    this.valueType = valueType;
    this.comparator = comparator;
    this.throwOnOther = true;
    this.valueOnOther = false;
  }

  public GenericComparators(
      StorageType<T> valueType, BiPredicate<T, T> comparator, boolean valueOnOther) {
    this.valueType = valueType;
    this.comparator = comparator;
    this.throwOnOther = false;
    this.valueOnOther = valueOnOther;
  }

  protected T asTypedValue(Object value) {
    return valueType.valueAsType(value);
  }

  protected ColumnStorage<T> asTypedStorage(ColumnStorage<?> storage) {
    return valueType.asTypedStorage(storage);
  }

  protected boolean onIncomparable(Object left, Object right) {
    if (throwOnOther) {
      throw new CompareException(left, right);
    }
    return valueOnOther;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return valueType.isOfType(left.getType());
  }

  @Override
  public ColumnStorage<Boolean> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    if (left.getType() instanceof NullType || rightValue == null) {
      return Builder.makeEmpty(BooleanType.INSTANCE, left.getSize());
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
    } else {
      // If all are Nothing then will return a Nothing Boolean Storage
      return StorageIterators.buildOverStorage(
          typedLeft,
          builder,
          (b, index, value) -> b.appendBoolean(onIncomparable(value, rightValue)));
    }
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return valueType.isOfType(left.getType())
        && (!throwOnOther
            || valueType.isOfType(right.getType())
            || right.getType() instanceof AnyObjectType);
  }

  @Override
  public ColumnStorage<Boolean> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (left.getType() instanceof NullType || right.getType() instanceof NullType) {
      var size = Math.max(left.getSize(), right.getSize());
      return Builder.makeEmpty(BooleanType.INSTANCE, size);
    }

    assert canApplyZip(left, right);

    var typedLeft = asTypedStorage(left);
    if (!valueType.isOfType(right.getType())) {
      // Fall back to iterating over each.
      return StorageIterators.zipOverStorages(
          typedLeft,
          right,
          Builder::getForBoolean,
          true,
          (index, leftValue, rightValue) -> {
            T typedRightValue = asTypedValue(rightValue);
            return typedRightValue == null
                ? onIncomparable(leftValue, rightValue)
                : comparator.test(leftValue, typedRightValue);
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
