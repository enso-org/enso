package org.enso.table.data.column.operation;

import org.enso.base.CompareException;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public abstract class BinaryOperationNumeric<T, R> implements BinaryOperationTyped<R> {
  protected static StorageType<?> storageTypeForObject(Object right) {
    if (right == null) {
      return NullType.INSTANCE;
    }

    if (right instanceof Column rightColumn) {
      return ColumnStorageWithInferredStorage.resolveStorage(rightColumn).getType();
    }

    return StorageType.forBoxedItem(right, PreciseTypeOptions.DEFAULT);
  }

  protected final NumericColumnAdapter<T> adapter;
  private final boolean preserveNulls;
  protected final boolean throwOnOther;
  protected final R valueOnOther;
  protected final StorageType<R> returnType;

  protected BinaryOperationNumeric(
      final NumericColumnAdapter<T> adapter,
      final boolean preserveNulls,
      final StorageType<R> returnType) {
    this.adapter = adapter;
    this.preserveNulls = preserveNulls;
    this.returnType = returnType;
    this.throwOnOther = true;
    this.valueOnOther = null;
  }

  protected BinaryOperationNumeric(
      final NumericColumnAdapter<T> adapter,
      final boolean preserveNulls,
      final StorageType<R> returnType,
      final R valueOnOther) {
    this.adapter = adapter;
    this.preserveNulls = preserveNulls;
    this.returnType = returnType;
    this.throwOnOther = false;
    this.valueOnOther = valueOnOther;
  }

  protected R onIncomparable(Object left, Object right) {
    if (throwOnOther) {
      throw new CompareException(left, right);
    }
    return valueOnOther;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return adapter.canApply(left);
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    if (!canApplyMap(left, null)) {
      return false;
    }

    // If not throwing on other types, we can apply the operation
    // Otherwise, we allow Any and Null types on the right or support type.
    if (!throwOnOther) {
      return true;
    }

    var rightType = right.getType();
    return switch (rightType) {
      case NullType nt -> true;
      case AnyObjectType at -> true;
      default -> canApplyMap(right, null);
    };
  }

  @Override
  public ColumnStorage<R> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    assert canApplyMap(left, rightValue);

    if (rightValue == null) {
      return applyNullMap(left, problemAggregator);
    }

    T rightValueTyped = adapter.getValidType().valueAsType(rightValue);
    if (rightValueTyped == null) {
      // If all are Nothing then will return a Nothing Boolean Storage
      return StorageIterators.buildOverStorage(
          left,
          returnType.makeBuilder(left.getSize(), problemAggregator),
          (b, index, value) -> b.append(onIncomparable(value, rightValue)));
    }

    return innerApplyMap(adapter.asTypedStorage(left), rightValueTyped, problemAggregator);
  }

  @Override
  public ColumnStorage<R> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    assert canApplyZip(left, right);

    if (NullType.INSTANCE.isOfType(right.getType())) {
      return applyNullMap(left, problemAggregator);
    }

    // Handle the case where right is Any or another type
    if (!adapter.getValidType().isOfType(right.getType())) {
      // Have a mismatch in types (could be AnyObjectType)
      return StorageIterators.zipOverStorages(
          adapter.asTypedStorage(left),
          right,
          size -> returnType.makeBuilder(size, problemAggregator),
          preserveNulls,
          (index, leftValue, rightValue) -> {
            T rightValueTyped = adapter.getValidType().valueAsType(rightValue);
            return rightValue != null && rightValueTyped == null
                ? onIncomparable(leftValue, rightValue)
                : doSingle(leftValue, rightValueTyped, index, problemAggregator);
          });
    }

    return innerApplyZip(
        adapter.asTypedStorage(left), adapter.asTypedStorage(right), problemAggregator);
  }

  protected abstract ColumnStorage<R> applyNullMap(
      ColumnStorage<?> left, MapOperationProblemAggregator problemAggregator);

  protected ColumnStorage<R> innerApplyMap(
      ColumnStorage<T> left, T right, MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.mapOverStorage(
        left,
        preserveNulls,
        returnType.makeBuilder(left.getSize(), problemAggregator),
        (index, value) -> doSingle(value, right, index, problemAggregator));
  }

  protected ColumnStorage<R> innerApplyZip(
      ColumnStorage<T> left,
      ColumnStorage<T> right,
      MapOperationProblemAggregator problemAggregator) {
    return StorageIterators.zipOverStorages(
        left,
        right,
        size -> returnType.makeBuilder(size, problemAggregator),
        preserveNulls,
        (index, x, y) -> doSingle(x, y, index, problemAggregator));
  }

  protected abstract R doSingle(
      T left, T right, long index, MapOperationProblemAggregator problemAggregator);
}
