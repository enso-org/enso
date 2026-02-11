package org.enso.table.data.column.operation;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public class BinaryOperationNull implements BinaryOperationTyped<Void> {
  public static final BinaryOperationNull INSTANCE = new BinaryOperationNull();

  private BinaryOperationNull() {}

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return StorageType.ofStorage(left) instanceof NullType;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return canApplyMap(left, null);
  }

  @Override
  public ColumnStorage<Void> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    if (StorageType.ofStorage(left) instanceof NullType) {
      return NullType.INSTANCE.asTypedStorage(left);
    }
    throw new IllegalArgumentException("Unsupported storage type.");
  }

  @Override
  public ColumnStorage<Void> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    if (left.getSize() != right.getSize()) {
      throw new IllegalArgumentException("Columns must be of the same size.");
    }

    return applyMap(left, right, problemAggregator);
  }
}
