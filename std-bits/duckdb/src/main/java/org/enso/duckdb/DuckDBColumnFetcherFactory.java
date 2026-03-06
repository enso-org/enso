package org.enso.duckdb;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.time.LocalTime;
import java.time.OffsetTime;
import org.enso.database.fetchers.ColumnFetcher;
import org.enso.database.fetchers.ColumnFetcherFactory;
import org.enso.database.fetchers.GenericColumnFetcher;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.problems.ProblemAggregator;

public class DuckDBColumnFetcherFactory extends ColumnFetcherFactory.DefaultColumnFetcherFactory {
  public static final ColumnFetcherFactory INSTANCE = new DuckDBColumnFetcherFactory();

  @Override
  public ColumnFetcher forStorageType(
      StorageType<?> storageType,
      int index,
      String columnName,
      ProblemAggregator problemAggregator) {
    // JDBC column indices are 1-based.
    int colIndex = index + 1;
    return switch (storageType) {
      case TimeOfDayType todt ->
          new GenericColumnFetcher<>(colIndex, columnName, todt, problemAggregator) {
            @Override
            public Object getValue(ResultSet resultSet) throws SQLException {
              var object = resultSet.getObject(index());
              if (object instanceof LocalTime lt) {
                return lt;
              } else if (object instanceof OffsetTime ot) {
                return ot.toLocalTime();
              } else if (object instanceof Time sqlt) {
                return sqlt.toLocalTime();
              } else {
                return null;
              }
            }
          };
      default -> super.forStorageType(storageType, index, columnName, problemAggregator);
    };
  }
}
