package org.enso.table.data.column.operation.unary;

import java.util.HashMap;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

/**
 * An operation that counts the number of occurrences of each value in a column. It returns a new
 * column where each entry is the running count of the corresponding value in the original column.
 */
public final class DuplicateCountOperation implements UnaryOperation {
  public static final DuplicateCountOperation INSTANCE = new DuplicateCountOperation();

  private DuplicateCountOperation() {}

  @Override
  public String getName() {
    return "duplicate_count";
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return true;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    final HashMap<Object, Long> counts = new HashMap<>();
    return StorageIterators.buildOverStorage(
        storage,
        IntegerType.INT_64.makeBuilder(storage.getSize(), problemAggregator),
        (builder, index, value) -> {
          long current = counts.getOrDefault(value, 0L);
          counts.put(value, current + 1);
          builder.append(current);
        });
  }
}
