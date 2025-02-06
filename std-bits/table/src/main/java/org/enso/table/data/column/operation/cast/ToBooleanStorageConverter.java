package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public class ToBooleanStorageConverter implements StorageConverter<Boolean> {
  @Override
  public boolean canApply(StorageType sourceType) {
    return sourceType.isNumeric()
        || sourceType instanceof AnyObjectType
        || sourceType instanceof NullType;
  }

  @Override
  public ColumnStorage<Boolean> cast(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    if (canApply(storage.getType())) {
      return castFromObject(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Boolean.");
    }
  }

  private ColumnStorage<Boolean> castFromObject(
      ColumnStorage<?> storage, CastProblemAggregator problemAggregator) {
    // As mixed storage is already boxed, use the standard inner loop.
    return StorageIterators.mapOverStorage(
        storage,
        Builder.getForBoolean(storage.getSize()),
        (index, value) ->
            switch (value) {
              case Boolean b -> b;
              case Number n -> n.doubleValue() != 0;
              default -> {
                problemAggregator.reportConversionFailure(value);
                yield null;
              }
            });
  }
}
