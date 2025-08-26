package org.enso.database.fetchers;

import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ProblemAggregator;

class GenericColumnFetcher<T> extends BaseColumnFetcher {
  GenericColumnFetcher(
      int index, String name, StorageType<T> storageType, ProblemAggregator problemAggregator) {
    super(index, name, storageType.makeBuilder(DEFAULT_SIZE, problemAggregator));
  }

  @Override
  public void append(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    builder.append(getValue(resultSet));
  }
}
