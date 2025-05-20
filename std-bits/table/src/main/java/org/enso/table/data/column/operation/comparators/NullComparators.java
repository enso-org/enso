package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;

public final class NullComparators implements BinaryOperation<Boolean> {
  public static final NullComparators INSTANCE = new NullComparators();

  private NullComparators() {}

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return left.getType() instanceof NullType;
  }

  @Override
  public ColumnStorage<Boolean> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator) {
    assert canApplyMap(left, rightValue);
    return BoolStorage.makeEmpty(left.getSize());
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return left.getType() instanceof NullType;
  }

  @Override
  public ColumnStorage<Boolean> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator) {
    assert canApplyZip(left, right);
    return BoolStorage.makeEmpty(left.getSize());
  }
}
