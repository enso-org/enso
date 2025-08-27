package org.enso.database.fetchers;

import java.sql.ResultSet;
import java.sql.SQLException;
import org.enso.database.JDBCUtils;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.problems.ProblemAggregator;

public interface ColumnFetcherFactory {
  ColumnFetcherFactory DEFAULT = new DefaultColumnFetcherFactory();

  ColumnFetcher forStorageType(
      StorageType<?> storageType,
      int index,
      String columnName,
      ProblemAggregator problemAggregator);

  class DefaultColumnFetcherFactory implements ColumnFetcherFactory {
    public ColumnFetcher forStorageType(
        StorageType<?> storageType,
        int index,
        String columnName,
        ProblemAggregator problemAggregator) {
      // JDBC column indices are 1-based.
      int colIndex = index + 1;
      return switch (storageType) {
        case BooleanType bt -> new BooleanColumnFetcher(colIndex, columnName);
        case IntegerType it -> new LongColumnFetcher(colIndex, columnName, it, problemAggregator);
        case FloatType ft -> new DoubleColumnFetcher(colIndex, columnName, ft, problemAggregator);
        case BigIntegerType bi -> new GenericColumnFetcher<>(
            colIndex, columnName, bi, problemAggregator) {
          @Override
          public Object getValue(ResultSet resultSet) throws SQLException {
            var bigDecimal = resultSet.getBigDecimal(index());
            return bigDecimal == null ? null : bigDecimal.toBigIntegerExact();
          }
        };
        case BigDecimalType bd -> new GenericColumnFetcher<>(
            colIndex, columnName, bd, problemAggregator) {
          @Override
          public Object getValue(ResultSet resultSet) throws SQLException {
            return JDBCUtils.getBigDecimalHandleSpecialFloats(resultSet, index());
          }
        };
        case TextType tt -> new GenericColumnFetcher<>(
            colIndex, columnName, tt, problemAggregator) {
          @Override
          public Object getValue(ResultSet resultSet) throws SQLException {
            return resultSet.getString(index());
          }
        };
        case TimeOfDayType todt -> new GenericColumnFetcher<>(
            colIndex, columnName, todt, problemAggregator) {
          @Override
          public Object getValue(ResultSet resultSet) throws SQLException {
            return JDBCUtils.getLocalTime(resultSet, index());
          }
        };
        case DateType dt -> new GenericColumnFetcher<>(
            colIndex, columnName, dt, problemAggregator) {
          @Override
          public Object getValue(ResultSet resultSet) throws SQLException {
            return JDBCUtils.getLocalDate(resultSet, index());
          }
        };
        case DateTimeType dtt -> dtt.hasTimeZone()
            ? new GenericColumnFetcher<>(
                colIndex, columnName, DateTimeType.INSTANCE, problemAggregator) {
              @Override
              public Object getValue(ResultSet resultSet) throws SQLException {
                return JDBCUtils.getLocalDateTimeAsZoned(resultSet, index());
              }
            }
            : new GenericColumnFetcher<>(
                colIndex, columnName, DateTimeType.INSTANCE, problemAggregator) {
              @Override
              public Object getValue(ResultSet resultSet) throws SQLException {
                return JDBCUtils.getZonedDateTime(resultSet, index());
              }
            };
        default -> new InferredColumnFetcher(colIndex, columnName, problemAggregator);
      };
    }
  }
}
