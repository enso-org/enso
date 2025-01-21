package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;

public class ToBooleanStorageConverter implements StorageConverter<Boolean> {
  @Override
  public Storage<Boolean> cast(Storage<?> storage, CastProblemAggregator problemAggregator) {
    if (storage instanceof BoolStorage boolStorage) {
      return boolStorage;
    } else if (storage.getType() instanceof AnyObjectType) {
      return castFromMixed(storage, problemAggregator);
    } else {
      throw new IllegalStateException(
          "No known strategy for casting storage " + storage + " to Boolean.");
    }
  }

  private Storage<Boolean> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    // As mixed storage is already boxed, use the standard inner loop.
    return StorageConverter.innerLoop(
        Builder.getForBoolean(mixedStorage.size()),
        mixedStorage,
        (i) -> {
          Object o = mixedStorage.getItemBoxed(i);
          if (o instanceof Boolean b) {
            return b;
          } else {
            problemAggregator.reportConversionFailure(o);
            return null;
          }
        });
  }
}
