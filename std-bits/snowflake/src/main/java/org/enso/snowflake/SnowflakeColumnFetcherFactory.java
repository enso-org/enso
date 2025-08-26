package org.enso.snowflake;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import org.enso.database.fetchers.BaseColumnFetcher;
import org.enso.database.fetchers.ColumnFetcher;
import org.enso.database.fetchers.ColumnFetcherFactory;
import org.enso.database.fetchers.GenericColumnFetcher;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.problems.ProblemAggregator;

public class SnowflakeColumnFetcherFactory extends ColumnFetcherFactory.DefaultColumnFetcherFactory {
  private static final String DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss.S[ XX][ XXXXX]";
  private static final DateTimeFormatter DATE_TIME_FORMATTER =
      DateTimeFormatter.ofPattern(DATE_TIME_FORMAT);

  private static class SnowflakeIntegerFetcher extends BaseColumnFetcher {
    SnowflakeIntegerFetcher(int index, String name) {
      super(index, name, new SnowflakeIntegerColumnMaterializer(DEFAULT_SIZE));
    }

    @Override
    public Object getValue(ResultSet resultSet) throws SQLException {
      return resultSet.getBigDecimal(index()).toBigIntegerExact();
    }

    @Override
    public void append(java.sql.ResultSet resultSet) throws java.sql.SQLException {
      builder.append(getValue(resultSet));
    }
  }

  @Override
  public ColumnFetcher forStorageType(
      StorageType<?> storageType,
      int index,
      String columnName,
      ProblemAggregator problemAggregator) {
    return switch (storageType) {
      case TimeOfDayType todt -> new GenericColumnFetcher<>(
          index + 1, columnName, todt, problemAggregator) {
        @Override
        public Object getValue(ResultSet resultSet) throws SQLException {
          var timeString = resultSet.getString(index());
          return timeString == null
              ? null
              : LocalTime.parse(timeString, DateTimeFormatter.ISO_LOCAL_TIME);
        }
      };
      case DateTimeType dtt -> new GenericColumnFetcher<>(
          index + 1, columnName, dtt, problemAggregator) {
        @Override
        public Object getValue(ResultSet resultSet) throws SQLException {
          var timestampString = resultSet.getString(index());
          if (timestampString == null) {
            return null;
          }

          var normalised =
              timestampString.charAt(10) == 'T'
                  ? timestampString.substring(0, 10) + ' ' + timestampString.substring(11)
                  : timestampString;
          return ZonedDateTime.parse(normalised, DATE_TIME_FORMATTER);
        }
      };
      case BigIntegerType bit -> new SnowflakeIntegerFetcher(index + 1, columnName);
      default -> super.forStorageType(storageType, index, columnName, problemAggregator);
    };
  }
}
