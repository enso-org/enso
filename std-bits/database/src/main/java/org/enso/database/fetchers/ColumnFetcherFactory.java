package org.enso.database.fetchers;

import java.lang.reflect.Proxy;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.problems.ProblemAggregator;

public interface ColumnFetcherFactory {
  ColumnFetcherFactory DEFAULT = new DefaultColumnFetcherFactory();

  /** Reads all values from a column in a ResultSet into a String array. */
  default String[] readTextColumn(ResultSet rs, String name) throws SQLException {
    if (rs.isClosed()) {
      return new String[0];
    }

    int index = rs.findColumn(name);

    var results = new ArrayList<String>();
    while (rs.next()) {
      var value = rs.getString(index);
      results.add(value);
    }
    return results.toArray(new String[0]);
  }

  default Class<? extends ColumnFetcher> getColumnFetcherClass() {
    return ColumnFetcher.class;
  }

  default Class<? extends ProblemAggregator> getAggregatorClass() {
    return ProblemAggregator.class;
  }

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
        case BigIntegerType bi ->
            new GenericColumnFetcher<>(colIndex, columnName, bi, problemAggregator) {
              @Override
              public Object getValue(ResultSet resultSet) throws SQLException {
                var bigDecimal = resultSet.getBigDecimal(index());
                return bigDecimal == null ? null : bigDecimal.toBigIntegerExact();
              }
            };
        case BigDecimalType bd ->
            new GenericColumnFetcher<>(colIndex, columnName, bd, problemAggregator) {
              @Override
              public Object getValue(ResultSet resultSet) throws SQLException {
                try {
                  return resultSet.getBigDecimal(index());
                } catch (SQLException e) {
                  try {
                    double d = resultSet.getDouble(index());
                    if (Double.isNaN(d) || Double.isInfinite(d)) {
                      return null;
                    } else {
                      throw e;
                    }
                  } catch (SQLException eIgnore) {
                    throw e;
                  }
                }
              }
            };
        case TextType tt ->
            new GenericColumnFetcher<>(colIndex, columnName, tt, problemAggregator) {
              @Override
              public Object getValue(ResultSet resultSet) throws SQLException {
                return resultSet.getString(index());
              }
            };
        case TimeOfDayType todt ->
            new GenericColumnFetcher<>(colIndex, columnName, todt, problemAggregator) {
              @Override
              public Object getValue(ResultSet resultSet) throws SQLException {
                return resultSet.getObject(index(), LocalTime.class);
              }
            };
        case DateType dt ->
            new GenericColumnFetcher<>(colIndex, columnName, dt, problemAggregator) {
              @Override
              public Object getValue(ResultSet resultSet) throws SQLException {
                var sqlDate = resultSet.getDate(index());
                return sqlDate == null ? null : sqlDate.toLocalDate();
              }
            };
        case DateTimeType dtt ->
            dtt.hasTimeZone()
                ? new GenericColumnFetcher<>(
                    colIndex, columnName, DateTimeType.INSTANCE, problemAggregator) {
                  @Override
                  public Object getValue(ResultSet resultSet) throws SQLException {
                    var localDateTime = resultSet.getObject(index(), LocalDateTime.class);
                    return localDateTime == null
                        ? null
                        : localDateTime.atZone(Core_Date_Utils.defaultSystemZone());
                  }
                }
                : new GenericColumnFetcher<>(
                    colIndex, columnName, DateTimeType.INSTANCE, problemAggregator) {
                  @Override
                  public Object getValue(ResultSet resultSet) throws SQLException {
                    var offsetDateTime = resultSet.getObject(index(), OffsetDateTime.class);
                    return offsetDateTime == null ? null : offsetDateTime.toZonedDateTime();
                  }
                };
        default -> {
          if (Proxy.isProxyClass(storageType.getClass())) {
            var fromProxy =
                StorageType.fromTypeCharAndSize(storageType.typeChar(), storageType.size());
            yield forStorageType(fromProxy, index, columnName, problemAggregator);
          } else {
            yield new InferredColumnFetcher(colIndex, columnName, problemAggregator);
          }
        }
      };
    }
  }
}
