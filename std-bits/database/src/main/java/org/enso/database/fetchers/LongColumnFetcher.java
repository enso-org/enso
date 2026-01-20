package org.enso.database.fetchers;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForLong;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.ProblemAggregator;

final class LongColumnFetcher extends BaseColumnFetcher {
  private final BuilderForLong longBuilder;

  LongColumnFetcher(
      int index, String name, IntegerType integerType, ProblemAggregator problemAggregator) {
    super(index, name, Builder.getForLong(integerType, DEFAULT_SIZE, problemAggregator));
    longBuilder = (BuilderForLong) builder;
  }

  @Override
  public void append(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    long longValue = resultSet.getLong(index());
    if (resultSet.wasNull()) {
      longBuilder.appendNulls(1);
    } else {
      longBuilder.appendLong(longValue);
    }
  }

  @Override
  public Object getValue(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    double longValue = resultSet.getLong(index());
    return resultSet.wasNull() ? null : longValue;
  }
}
