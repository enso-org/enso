package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithValidityMap;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;
import org.enso.table.util.ImmutableBitSet;

public class IsNothingOperation implements UnaryOperation {
  public static final String NAME = "is_nothing";
  public static final UnaryOperation INSTANCE = new IsNothingOperation();

  private IsNothingOperation() {}

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return true;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnStorageWithValidityMap validityMap) {
      var size = (int) storage.getSize();
      var allValidity = ImmutableBitSet.allTrue(size);
      return new BoolStorage(validityMap.getValidityMap(), allValidity, size, true, null);
    }

    return StorageIterators.buildOverStorage(
        storage,
        false,
        Builder.getForBoolean(storage.getSize()),
        (builder, index, value) -> builder.appendBoolean(value == null));
  }
}
