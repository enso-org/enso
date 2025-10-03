package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public abstract class BinaryOperationBase<T, R> implements BinaryOperationTyped<R> {
  protected final StorageType<T> validType;
  protected final StorageType<R> returnType;
  private final boolean allowNullType;

  protected BinaryOperationBase(
      StorageType<T> validType, StorageType<R> returnType, boolean allowNullType) {
    this.validType = validType;
    this.returnType = returnType;
    this.allowNullType = allowNullType;
  }

  @Override
  public final boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    var leftType = left.getType();
    return validType.isOfType(leftType) || (allowNullType && NullType.INSTANCE.isOfType(leftType));
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return canApplyMap(left, null)
        && (NullType.INSTANCE.isOfType(right.getType()) || canApplyMap(right, null));
  }

  @Override
  public ColumnStorage<R> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    if (left.getType() instanceof NullType) {
      return applyNullMap(left, rightValue, problemAggregator);
    }

    if (validType.isOfType(left.getType())) {
      return applyTypedMap(validType.asTypedStorage(left), rightValue, problemAggregator);
    }

    throw new IllegalArgumentException("Unsupported storage type.");
  }

  protected ColumnStorage<R> applyNullMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    throw new IllegalArgumentException(
        "applyNullMap has not been implemented. This is a bug in the libraries code.");
  }

  protected ColumnStorage<R> applyTypedMap(
      ColumnStorage<T> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    throw new IllegalArgumentException(
        "applyTypedMap has not been implemented. This is a bug in the libraries code.");
  }

  @Override
  public ColumnStorage<R> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (left.getSize() != right.getSize()) {
      throw new IllegalArgumentException("Columns must be of the same size.");
    }

    if (left.getType() instanceof NullType || right.getType() instanceof NullType) {
      return applyNullMap(left, right, problemAggregator);
    }

    if (validType.isOfType(left.getType())) {
      return applyTypedZip(validType.asTypedStorage(left), right, problemAggregator);
    }

    throw new IllegalArgumentException("Unsupported storage types.");
  }

  protected ColumnStorage<R> applyTypedZip(
      ColumnStorage<T> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    throw new IllegalArgumentException(
        "applyTypedZip has not been implemented. This is a bug in the libraries code.");
  }

  protected BuilderForType<R> makeStorageBuilder(
      long size,
      StorageType<?> leftType,
      StorageType<?> rightType,
      ProblemAggregator problemAggregator) {
    return returnType.makeBuilder(size, problemAggregator);
  }
}
