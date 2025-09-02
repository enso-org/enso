package org.enso.database.fetchers;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForDouble;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.problems.ProblemAggregator;

final class DoubleColumnFetcher extends BaseColumnFetcher {
  private final BuilderForDouble doubleBuilder;

  DoubleColumnFetcher(
      int index, String name, FloatType floatType, ProblemAggregator problemAggregator) {
    super(index, name, Builder.getForDouble(floatType, DEFAULT_SIZE, problemAggregator));
    doubleBuilder = (BuilderForDouble) builder;
  }

  @Override
  public void append(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    double doubleValue = resultSet.getDouble(index());
    if (resultSet.wasNull()) {
      doubleBuilder.appendNulls(1);
    } else {
      doubleBuilder.appendDouble(doubleValue);
    }
  }

  @Override
  public Object getValue(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    double doubleValue = resultSet.getDouble(index());
    return resultSet.wasNull() ? null : doubleValue;
  }
}
