package org.enso.database.fetchers;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.function.BiFunction;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;

public interface ColumnFetcher {
  private static ColumnFetcher[] forResultSet(
      ResultSet rs,
      ProblemAggregator problemAggregator,
      BiFunction<ResultSetMetaData, Integer, StorageType<?>> storageTypeMapper,
      ColumnFetcherFactory factory)
      throws SQLException {
    var meta = rs.getMetaData();
    int columnCount = meta.getColumnCount();
    ColumnFetcher[] fetchers = new ColumnFetcher[columnCount];
    for (int i = 0; i < columnCount; i++) {
      String columnName = meta.getColumnName(i + 1);

      var storageType = storageTypeMapper.apply(meta, i);
      fetchers[i] = factory.forStorageType(storageType, i, columnName, problemAggregator);
    }
    return fetchers;
  }

  private static Table getTable(ColumnFetcher[] fetchers) {
    Column[] columns = new Column[fetchers.length];
    for (int i = 0; i < fetchers.length; i++) {
      columns[i] = fetchers[i].seal();
    }
    return new Table(columns);
  }

  static Table readResultSet(
      ResultSet rs,
      int rowLimit,
      ProblemAggregator problemAggregator,
      BiFunction<ResultSetMetaData, Integer, StorageType<?>> storageTypeMapper,
      ColumnFetcherFactory factory)
      throws SQLException {
    // Create the fetchers
    var fetchers = forResultSet(rs, problemAggregator, storageTypeMapper, factory);

    while (rowLimit != 0 && rs.next()) {
      for (var fetcher : fetchers) {
        fetcher.append(rs);
      }

      rowLimit -= 1;
    }

    return getTable(fetchers);
  }

  static Table readLastRow(
      ResultSet rs,
      ProblemAggregator problemAggregator,
      BiFunction<ResultSetMetaData, Integer, StorageType<?>> storageTypeMapper,
      ColumnFetcherFactory factory)
      throws SQLException {
    // Create the fetchers
    var fetchers = forResultSet(rs, problemAggregator, storageTypeMapper, factory);

    if (rs.getType() != ResultSet.TYPE_FORWARD_ONLY && rs.last()) {
      for (var fetcher : fetchers) {
        fetcher.append(rs);
      }
    } else {
      var lastValues = new Object[fetchers.length];
      while (rs.next()) {
        for (int i = 0; i < fetchers.length; i++) {
          lastValues[i] = fetchers[i].getValue(rs);
        }
      }

      for (int i = 0; i < fetchers.length; i++) {
        fetchers[i].appendValue(lastValues[i]);
      }
    }

    return getTable(fetchers);
  }

  /**
   * Gets the name of the column being fetched.
   *
   * @return the column name
   */
  String name();

  /**
   * Appends all values from the given ResultSet to the fetcher. The ResultSet should be positioned
   * before the first row.
   *
   * @param resultSet the ResultSet to fetch from
   * @throws SQLException if a database access error occurs
   */
  void append(ResultSet resultSet) throws SQLException;

  /**
   * Seals the fetcher and returns a column with the fetched data.
   *
   * @return the sealed column
   */
  Column seal();

  /** Fetches a value from the ResultSet at the fetcher's index. */
  Object getValue(ResultSet resultSet) throws SQLException;

  /** Appends a value directly to the fetcher. */
  void appendValue(Object value);
}
