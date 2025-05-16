package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.BlackholeProblemAggregator;

public abstract class BinaryOperationBase<T> implements BinaryOperation<T> {
  protected final StorageType<T> validType;

  protected BinaryOperationBase(StorageType<T> validType) {
    this.validType = validType;
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

  protected BuilderForType<T> makeStorageBuilder(
      long size, StorageType<?> leftType, StorageType<?> rightType) {
    return validType.makeBuilder(size, BlackholeProblemAggregator.INSTANCE);
  }
}
