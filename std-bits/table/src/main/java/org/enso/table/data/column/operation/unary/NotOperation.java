package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.NullStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;

public class NotOperation implements UnaryOperation {
  public static final String NAME = "not";

  public static final UnaryOperation INSTANCE = new NotOperation();

  private NotOperation() {}

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return storage.getType() instanceof BooleanType || storage.getType() instanceof NullType;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof BoolStorage boolStorage) {
      return boolStorage.makeNegated();
    }

    if (storage.getType() instanceof NullType) {
      return new NullStorage(Math.toIntExact(storage.getSize()));
    }

    if (storage instanceof ColumnBooleanStorage booleanStorage) {
      return StorageIterators.buildOverBooleanStorage(
          booleanStorage,
          Builder.getForBoolean(storage.getSize()),
          (builder, index, value, isNothing) -> builder.appendBoolean(!value));
    } else {
      return StorageIterators.buildOverStorage(
          storage,
          Builder.getForBoolean(storage.getSize()),
          (builder, index, value) -> {
            if (value instanceof Boolean b) {
              builder.appendBoolean(!b);
            } else {
              throw new IllegalArgumentException(
                  "Unsupported type: " + value.getClass() + " (expected boolean type).");
            }
          });
    }
  }
}
