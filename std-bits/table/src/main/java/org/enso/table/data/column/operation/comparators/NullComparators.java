package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

final class NullComparators implements BinaryOperationTyped<Boolean> {
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
    return Builder.makeEmpty(BooleanType.INSTANCE, left.getSize());
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
    return Builder.makeEmpty(BooleanType.INSTANCE, left.getSize());
  }
}
