package org.enso.database.fetchers;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.function.BiFunction;
import org.enso.database.JDBCUtils;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;

public interface ColumnFetcher {
  static ColumnFetcher forStorageType(
      StorageType<?> storageType,
      int index,
      String columnName,
      ProblemAggregator problemAggregator) {
    return switch (storageType) {
      case BooleanType bt -> new BooleanColumnFetcher(index + 1, columnName);
      case IntegerType it -> new LongColumnFetcher(index + 1, columnName, it, problemAggregator);
      case FloatType ft -> new DoubleColumnFetcher(index + 1, columnName, ft, problemAggregator);
      case BigIntegerType bi -> new GenericColumnFetcher<>(
          index + 1, columnName, bi, problemAggregator) {
        @Override
        public Object getValue(ResultSet resultSet) throws SQLException {
          return resultSet.getBigDecimal(index());
        }
      };
      case BigDecimalType bd -> new GenericColumnFetcher<>(
          index + 1, columnName, bd, problemAggregator) {
        @Override
        public Object getValue(ResultSet resultSet) throws SQLException {
          return JDBCUtils.getBigDecimalHandleSpecialFloats(resultSet, index());
        }
      };
      case TextType tt -> new GenericColumnFetcher<>(index + 1, columnName, tt, problemAggregator) {
        @Override
        public Object getValue(ResultSet resultSet) throws SQLException {
          return resultSet.getString(index());
        }
      };
      case TimeOfDayType todt -> new GenericColumnFetcher<>(
          index + 1, columnName, todt, problemAggregator) {
        @Override
        public Object getValue(ResultSet resultSet) throws SQLException {
          return JDBCUtils.getLocalTime(resultSet, index());
        }
      };
      case DateType dt -> new GenericColumnFetcher<>(index + 1, columnName, dt, problemAggregator) {
        @Override
        public Object getValue(ResultSet resultSet) throws SQLException {
          return JDBCUtils.getLocalDate(resultSet, index());
        }
      };
      case DateTimeType dtt -> dtt.hasTimeZone()
          ? new GenericColumnFetcher<>(
              index + 1, columnName, DateTimeType.INSTANCE, problemAggregator) {
            @Override
            public Object getValue(ResultSet resultSet) throws SQLException {
              return JDBCUtils.getLocalDateTimeAsZoned(resultSet, index());
            }
          }
          : new GenericColumnFetcher<>(
              index + 1, columnName, DateTimeType.INSTANCE, problemAggregator) {
            @Override
            public Object getValue(ResultSet resultSet) throws SQLException {
              return JDBCUtils.getZonedDateTime(resultSet, index());
            }
          };
      default -> new InferredColumnFetcher(index + 1, columnName, problemAggregator);
    };
  }

  static ColumnFetcher[] forResultSet(
      ResultSet rs,
      ProblemAggregator problemAggregator,
      BiFunction<ResultSetMetaData, Integer, StorageType<?>> storageTypeMapper)
      throws SQLException {
    var meta = rs.getMetaData();
    int columnCount = meta.getColumnCount();
    ColumnFetcher[] fetchers = new ColumnFetcher[columnCount];
    for (int i = 0; i < columnCount; i++) {
      String columnName = meta.getColumnName(i + 1);

      var storageType = storageTypeMapper.apply(meta, i);
      fetchers[i] = forStorageType(storageType, i, columnName, problemAggregator);
    }
    return fetchers;
  }

  static Table readResultSet(ResultSet rs, ColumnFetcher[] fetchers, int rowLimit)
      throws SQLException {
    while (rowLimit != 0 && rs.next()) {
      for (var fetcher : fetchers) {
        fetcher.append(rs);
      }

      rowLimit -= 1;
    }

    return getTable(fetchers);
  }

  private static Table getTable(ColumnFetcher[] fetchers) {
    Column[] columns = new Column[fetchers.length];
    for (int i = 0; i < fetchers.length; i++) {
      columns[i] = fetchers[i].seal();
    }
    return new Table(columns);
  }

  static Table readLastRow(ResultSet rs, ColumnFetcher[] fetchers) throws SQLException {
    if (rs.getType() != ResultSet.TYPE_FORWARD_ONLY) {
      if (rs.last()) {
        for (var fetcher : fetchers) {
          fetcher.append(rs);
        }
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
