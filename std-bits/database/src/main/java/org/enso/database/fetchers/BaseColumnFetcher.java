package org.enso.database.fetchers;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.table.Column;

public abstract class BaseColumnFetcher implements ColumnFetcher {
  protected static final int DEFAULT_SIZE = 1024;

  private final int index;
  private final String name;
  protected final Builder builder;

  protected BaseColumnFetcher(int index, String name, Builder builder) {
    this.index = index;
    this.name = name;
    this.builder = builder;
  }

  protected int index() {
    return index;
  }

  @Override
  public String name() {
    return name;
  }

  @Override
  public org.enso.table.data.table.Column seal() {
    return new Column(name(), builder.seal());
  }

  @Override
  public Object getValue(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    var value = resultSet.getObject(index());

    // Convert SQL date/time types to Java 8+ time types
    if (value instanceof java.sql.Date dataValue) {
      value = dataValue.toLocalDate();
    } else if (value instanceof java.sql.Time timeValue) {
      value = timeValue.toLocalTime();
    } else if (value instanceof java.sql.Timestamp timestampValue) {
      value = timestampValue.toLocalDateTime();
    }

    return value;
  }

  @Override
  public void append(java.sql.ResultSet resultSet) throws java.sql.SQLException {
    appendValue(getValue(resultSet));
  }

  @Override
  public void appendValue(Object value) {
    if (value == null) {
      builder.appendNulls(1);
    } else {
      builder.append(value);
    }
  }
}
