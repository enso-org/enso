package org.enso.duckdb;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import org.duckdb.DuckDBConnection;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;

public class DuckDBUtils {
  /**
   * Retrieves the version of the connected DuckDB database.
   *
   * @param connection an active DuckDBConnection
   * @return the version string of the DuckDB database
   */
  public static String getVersion(DuckDBConnection connection) {
    try (var stmt = connection.createStatement();
        var rs = stmt.executeQuery("SELECT version()")) {
      if (rs.next()) {
        return rs.getString(1);
      }
      return null;
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Appends the data from the given table to the specified target table in DuckDB.
   *
   * @param connection an active DuckDBConnection
   * @param table the source table containing data to append
   * @param targetTableName the name of the target table in DuckDB
   * @param targetTypes a list of StorageType representing the types of the target table columns
   * @return the number of rows appended
   * @throws SQLException if a database access error occurs
   */
  public static long append(
      DuckDBConnection connection,
      Table table,
      String targetTableName,
      List<StorageType<?>> targetTypes)
      throws SQLException {
    long rowCount = table.rowCount();
    if (rowCount == 0) {
      return 0;
    }

    var columns =
        Arrays.stream(table.getColumns()).map(Column::getStorage).toArray(ColumnStorage<?>[]::new);
    if (columns.length != targetTypes.size()) {
      throw new IllegalArgumentException(
          "Column count mismatch: table has "
              + columns.length
              + " columns, target types has "
              + targetTypes.size()
              + " types.");
    }

    try (var appender =
        connection.createAppender(
            connection.getCatalog(), connection.getSchema(), targetTableName)) {
      for (long i = 0; i < rowCount; i++) {
        appender.beginRow();
        for (int col = 0; col < columns.length; col++) {
          var column = columns[col];
          if (column.isNothing(i)) {
            appender.appendNull();
          } else if (column instanceof ColumnLongStorage longStorage) {
            // Could be a long, int, short, byte
            long value = longStorage.getItemAsLong(i);
            var storageType = targetTypes.get(col);
            if (storageType instanceof IntegerType integerType) {
              switch (integerType.bits()) {
                case BITS_8 -> appender.append(toByteExact(value));
                case BITS_16 -> appender.append(toShortExact(value));
                case BITS_32 -> appender.append(Math.toIntExact(value));
                case BITS_64 -> appender.append(value);
                default ->
                    throw new IllegalArgumentException(
                        "Unsupported integer bit size: " + integerType.bits());
              }
            } else {
              // Fallback and let Appended decide if its okay
              appender.append(longStorage.getItemAsLong(i));
            }
          } else if (column instanceof ColumnDoubleStorage doubleStorage) {
            // Could be a float or a double column
            double value = doubleStorage.getItemAsDouble(i);
            var storageType = targetTypes.get(col);
            if (storageType instanceof FloatType floatType && floatType.bits() == Bits.BITS_32) {
              appender.append((float) value);
            } else {
              appender.append(value);
            }
          } else if (column instanceof ColumnBooleanStorage boolStorage) {
            appender.append(boolStorage.getItemAsBoolean(i));
          } else {
            var value = column.getItemBoxed(i);
            switch (value) {
              case LocalDate localDate -> appender.append(localDate);
              case LocalTime localTime -> appender.append(localTime);
              case ZonedDateTime zonedDateTime -> {
                var storageType = targetTypes.get(col);
                if (storageType instanceof DateTimeType dateTimeType) {
                  if (dateTimeType.hasTimeZone()) {
                    OffsetDateTime offsetDateTime = zonedDateTime.toOffsetDateTime();
                    appender.append(offsetDateTime);
                  } else {
                    LocalDateTime localDateTime = zonedDateTime.toLocalDateTime();
                    appender.append(localDateTime);
                  }
                } else {
                  throw new IllegalArgumentException(
                      "Unsupported DateTime target type: " + storageType);
                }
              }
              case String string -> appender.append(string);
              case BigDecimal bigDecimal -> {
                var storageType = targetTypes.get(col);
                if (storageType instanceof BigDecimalType bigDecimalType) {
                  appender.append(
                      bigDecimal.setScale(bigDecimalType.getScale(), RoundingMode.HALF_UP));
                } else {
                  throw new IllegalArgumentException(
                      "Unsupported BigDecimal target type: " + storageType);
                }
              }
              case BigInteger bigInteger ->
                  appender.append(new BigDecimal(bigInteger).setScale(0, RoundingMode.UNNECESSARY));
              default ->
                  throw new IllegalArgumentException(
                      "Unsupported column type: " + column.getClass());
            }
          }
        }

        appender.endRow();
      }
    }

    return rowCount;
  }

  private static byte toByteExact(long value) {
    byte value_as_byte = (byte) value;
    if (value_as_byte != value) {
      throw new ArithmeticException("byte overflow " + value);
    }
    return value_as_byte;
  }

  private static short toShortExact(long value) {
    short value_as_short = (short) value;
    if (value_as_short != value) {
      throw new ArithmeticException("short overflow " + value);
    }
    return value_as_short;
  }
}
