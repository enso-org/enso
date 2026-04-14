package org.enso.database;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Value;

/** Java-side implementation of JDBC batch inserts for in-memory table uploads. */
public final class JDBCBatchInsert {
  private JDBCBatchInsert() {}

  public static void batchInsert(
      Connection connection,
      String insertTemplate,
      JDBCValueSetter jdbcValueSetter,
      Value columns,
      int batchSize,
      Value dateTimeWithTimezone,
      Value sqlTypeIds,
      int numRows)
      throws SQLException {
    try (PreparedStatement stmt = connection.prepareStatement(insertTemplate)) {
      int columnCount = Math.toIntExact(columns.getArraySize());

      for (int rowId = 0; rowId < numRows; rowId++) {
        for (int columnId = 0; columnId < columnCount; columnId++) {
          var columnValue = columns.getArrayElement(columnId);
          var column = columnValue.asHostObject();
          if (!(column instanceof Column javaColumn)) {
            throw new IllegalStateException("Expected Java table columns for JDBC batch insert.");
          }

          boolean keepTimezone = dateTimeWithTimezone.getArrayElement(columnId).asBoolean();
          int sqlTypeId = sqlTypeIds.getArrayElement(columnId).asInt();
          var value = javaColumn.getItem(rowId);
          setStatementValue(
              stmt, columnId + 1, value, jdbcValueSetter, keepTimezone, sqlTypeId);
        }

        stmt.addBatch();
        if ((rowId + 1) % batchSize == 0) {
          checkRows(stmt.executeBatch(), batchSize);
        }
      }

      int remainingRows = numRows % batchSize;
      if (remainingRows != 0) {
        checkRows(stmt.executeBatch(), remainingRows);
      }
    }
  }

  private static void checkRows(int[] updates, int expectedSize) {
    if (updates.length != expectedSize) {
      throw new IllegalStateException(
          "The batch update unexpectedly affected "
              + updates.length
              + " rows instead of "
              + expectedSize
              + " rows.");
    }

    for (int affectedRows : updates) {
      if (affectedRows != 1) {
        throw new IllegalStateException(
            "A single update within the batch unexpectedly affected "
                + affectedRows
                + " rows.");
      }
    }
  }

  private static void setStatementValue(
      PreparedStatement stmt,
      int columnIndex,
      Object value,
      JDBCValueSetter jdbcValueSetter,
      boolean dateTimeWithTimezone,
      int sqlTypeId)
      throws SQLException {
    if (value == null) {
      int nullType = jdbcValueSetter.databaseName().equals("SQLServer") ? sqlTypeId : Types.NULL;
      stmt.setNull(columnIndex, nullType);
      return;
    }

    switch (value) {
      case Boolean boolValue -> stmt.setBoolean(columnIndex, boolValue);
      case Byte byteValue -> stmt.setLong(columnIndex, byteValue.longValue());
      case Short shortValue -> stmt.setLong(columnIndex, shortValue.longValue());
      case Integer integerValue -> stmt.setLong(columnIndex, integerValue.longValue());
      case Long longValue -> stmt.setLong(columnIndex, longValue);
      case BigInteger bigIntegerValue ->
          jdbcValueSetter.setBigDecimal(stmt, columnIndex, bigIntegerValue, 0);
      case Float floatValue -> setFloatingPointValue(stmt, columnIndex, floatValue, jdbcValueSetter);
      case Double doubleValue ->
          setFloatingPointValue(stmt, columnIndex, doubleValue, jdbcValueSetter);
      case BigDecimal bigDecimalValue -> stmt.setBigDecimal(columnIndex, bigDecimalValue);
      case String textValue -> stmt.setString(columnIndex, textValue);
      case ZonedDateTime zonedDateTime ->
          setDateTimeValue(
              stmt, columnIndex, zonedDateTime, jdbcValueSetter, dateTimeWithTimezone);
      case LocalTime localTime -> jdbcValueSetter.setLocalTime(stmt, columnIndex, localTime);
      case LocalDate localDate -> jdbcValueSetter.setLocalDate(stmt, columnIndex, localDate);
      default -> stmt.setObject(columnIndex, value);
    }
  }

  private static void setFloatingPointValue(
      PreparedStatement stmt, int columnIndex, double value, JDBCValueSetter jdbcValueSetter)
      throws SQLException {
    if (jdbcValueSetter.databaseName().equals("SQLServer")
        && (Double.isNaN(value) || Double.isInfinite(value))) {
      stmt.setNull(columnIndex, Types.REAL);
    } else {
      stmt.setDouble(columnIndex, value);
    }
  }

  private static void setDateTimeValue(
      PreparedStatement stmt,
      int columnIndex,
      ZonedDateTime zonedDateTime,
      JDBCValueSetter jdbcValueSetter,
      boolean dateTimeWithTimezone)
      throws SQLException {
    if (dateTimeWithTimezone) {
      jdbcValueSetter.setZonedDateTime(stmt, columnIndex, zonedDateTime);
    } else {
      jdbcValueSetter.setLocalDateTime(stmt, columnIndex, zonedDateTime);
    }
  }
}
