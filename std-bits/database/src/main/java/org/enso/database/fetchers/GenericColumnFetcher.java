package org.enso.database.fetchers;

import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

public class GenericColumnFetcher<T> extends BaseColumnFetcher {
  public GenericColumnFetcher(
      int index, String name, StorageType<T> storageType, ProblemAggregator problemAggregator) {
    super(index, name, storageType.makeBuilder(DEFAULT_SIZE, problemAggregator));
  }
}
