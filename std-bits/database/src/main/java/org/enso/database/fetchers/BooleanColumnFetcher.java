package org.enso.database.fetchers;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForBoolean;

final class BooleanColumnFetcher extends BaseColumnFetcher {
  private final BuilderForBoolean boolBuilder;

  BooleanColumnFetcher(int index, String name) {
    super(index, name, Builder.getForBoolean(DEFAULT_SIZE));
    boolBuilder = (BuilderForBoolean) builder;
  }

  @Override
  public void append(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    boolean boolValue = resultSet.getBoolean(index());
    if (resultSet.wasNull()) {
      boolBuilder.appendNulls(1);
    } else {
      boolBuilder.appendBoolean(boolValue);
    }
  }

  @Override
  public Object getValue(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    boolean boolValue = resultSet.getBoolean(index());
    return resultSet.wasNull() ? null : boolValue;
  }
}
