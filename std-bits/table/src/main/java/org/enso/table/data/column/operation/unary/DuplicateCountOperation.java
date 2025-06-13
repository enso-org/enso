package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.MixedStorageFacade;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

import java.util.HashMap;

public final class DuplicateCountOperation implements UnaryOperation {
  public static final DuplicateCountOperation INSTANCE = new DuplicateCountOperation();

  @Override
  public String getName() {
    return "duplicate_count";
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return true;
  }

  @Override
  public ColumnStorage<?> apply(ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    var asObjectStorage = new MixedStorageFacade(storage);
    final HashMap<Object, Long> counts = new HashMap<>();
    return StorageIterators.buildOverStorage(
        asObjectStorage,
        IntegerType.INT_64.makeBuilder(storage.getSize(), problemAggregator),
        (builder, index, value) -> {
          var count = counts.put(value, counts.getOrDefault(value, 0) + 1);
          builder.append(count);
        });
  }
}
