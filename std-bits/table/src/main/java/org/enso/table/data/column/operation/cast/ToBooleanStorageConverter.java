package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.builder.BoolBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.graalvm.polyglot.Context;

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

  public Storage<Boolean> castFromMixed(
      Storage<?> mixedStorage, CastProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    BoolBuilder builder = new BoolBuilder(mixedStorage.size());
    for (int i = 0; i < mixedStorage.size(); i++) {
      Object o = mixedStorage.getItemBoxed(i);
      switch (o) {
        case null -> builder.appendNulls(1);
        case Boolean b -> builder.appendBoolean(b);
        default -> {
          problemAggregator.reportConversionFailure(o);
          builder.appendNulls(1);
        }
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
