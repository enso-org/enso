package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;

public final class NullComparators implements Comparators {
  public static final NullComparators INSTANCE = new NullComparators();

  private NullComparators() {}

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return left.getType() instanceof NullType;
  }

  @Override
  public ColumnStorage<Boolean> applyMap(ColumnStorage<?> left, Object rightValue) {
    assert canApplyMap(left, rightValue);
    return BoolStorage.makeEmpty(left.getSize());
  }

  @Override
  public boolean canApply(ColumnStorage<?> left, ColumnStorage<?> right) {
    return left.getType() instanceof NullType;
  }

  @Override
  public ColumnStorage<Boolean> apply(ColumnStorage<?> left, ColumnStorage<?> right) {
    assert canApply(left, right);
    return BoolStorage.makeEmpty(left.getSize());
  }
}
