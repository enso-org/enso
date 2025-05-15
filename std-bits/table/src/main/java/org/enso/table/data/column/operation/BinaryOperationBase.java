package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

public abstract class BinaryOperationBase<T> implements BinaryOperation<T> {
  protected final StorageType<T> validType;
  private final boolean allowNullType;

  protected BinaryOperationBase(StorageType<T> validType, boolean allowNullType) {
    this.validType = validType;
    this.allowNullType = allowNullType;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    var leftType = left.getType();
    return validType.isOfType(leftType) || (allowNullType && NullType.INSTANCE.isOfType(leftType));
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return canApplyMap(left, null)
        && (NullType.INSTANCE.isOfType(right.getType()) || canApplyMap(right, null));
  }

  protected BuilderForType<T> makeStorageBuilder(
      long size,
      StorageType<?> leftType,
      StorageType<?> rightType,
      ProblemAggregator problemAggregator) {
    return validType.makeBuilder(size, problemAggregator);
  }
}
