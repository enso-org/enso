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
      return switch (storageType) {
        case BooleanType bt -> new BooleanColumnFetcher(index + 1, columnName);
        case IntegerType it -> new LongColumnFetcher(index + 1, columnName, it, problemAggregator);
        case FloatType ft -> new DoubleColumnFetcher(index + 1, columnName, ft, problemAggregator);
        case BigIntegerType bi -> new GenericColumnFetcher<>(
            index + 1, columnName, bi, problemAggregator) {
          @Override
          public Object getValue(ResultSet resultSet) throws SQLException {
            var bigDecimal = resultSet.getBigDecimal(index());
            return bigDecimal == null ? null : bigDecimal.toBigIntegerExact();
          }
        };
        case BigDecimalType bd -> new GenericColumnFetcher<>(
            index + 1, columnName, bd, problemAggregator) {
          @Override
          public Object getValue(ResultSet resultSet) throws SQLException {
            return JDBCUtils.getBigDecimalHandleSpecialFloats(resultSet, index());
          }
        };
        case TextType tt -> new GenericColumnFetcher<>(
            index + 1, columnName, tt, problemAggregator) {
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
        case DateType dt -> new GenericColumnFetcher<>(
            index + 1, columnName, dt, problemAggregator) {
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
  }
}
