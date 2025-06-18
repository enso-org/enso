package org.enso.table.data.column.operation;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

/**
 * An interface for binary operations that can be applied to columns with a specific return type.
 *
 * @param <T> the type of the result of the operation
 */
public interface BinaryOperationTyped<T> extends BinaryOperation {
  @Override
  ColumnStorage<T> applyMap(
      ColumnStorage<?> left, Object rightValue, MapOperationProblemAggregator problemAggregator);

  @Override
  ColumnStorage<T> applyZip(
      ColumnStorage<?> left,
      ColumnStorage<?> right,
      MapOperationProblemAggregator problemAggregator);
}
