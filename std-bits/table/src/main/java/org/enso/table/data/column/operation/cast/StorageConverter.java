package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.StorageType;

/** A strategy for converting storages to a specific target type. */
public interface StorageConverter<T> {
  /** Given a storage can we convert using the converter? */
  boolean canApply(StorageType sourceType);

  /** Convert a given storage to the target type of this converter, reporting any problems. */
  ColumnStorage<T> cast(ColumnStorage<?> storage, CastProblemAggregator problemAggregator);
}
