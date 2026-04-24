package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

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
    var storageType = StorageType.ofStorage(storage);
    return storageType instanceof BooleanType || storageType instanceof NullType;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof BoolStorage boolStorage) {
      return applySpecializedBoolStorage(boolStorage);
    }

    var storageType = StorageType.ofStorage(storage);
    return switch (storageType) {
      case BooleanType _ -> applyOverBooleans(BooleanType.INSTANCE.asTypedStorage(storage));
      case NullType _ -> applySpecializedNullStorage(storage);
      default ->
          StorageIterators.buildOverStorage(
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
    };
  }

  public static ColumnStorage<Boolean> applyOverBooleans(ColumnBooleanStorage booleanStorage) {
    return StorageIterators.buildOverBooleanStorage(
        booleanStorage,
        Builder.getForBoolean(booleanStorage.getSize()),
        (builder, index, value, isNothing) -> builder.appendBoolean(!value));
  }

  public static ColumnBooleanStorage applySpecializedBoolStorage(BoolStorage boolStorage) {
    return new BoolStorage(
        boolStorage.getValues(),
        boolStorage.getValidityMap(),
        (int) boolStorage.getSize(),
        !boolStorage.isNegated(),
        null);
  }

  public static ColumnBooleanStorage applySpecializedNullStorage(ColumnStorage<?> storage) {
    return BooleanType.INSTANCE.asTypedStorage(
        Builder.makeEmpty(BooleanType.INSTANCE, storage.getSize()));
  }
}
