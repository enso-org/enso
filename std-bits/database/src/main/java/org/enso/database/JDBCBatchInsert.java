package org.enso.database;

import static java.util.Arrays.stream;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLTimeoutException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.base.polyglot.EnsoExceptionWrapper;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.graalvm.polyglot.Value;

/** Java-side implementation of JDBC batch inserts for in-memory table uploads. */
public final class JDBCBatchInsert {
  public Value batchInsert(
      Connection connection,
      String insertTemplate,
      JDBCValueSetter jdbcValueSetter,
      ColumnStorage<?>[] storages,
      int batchSize,
      boolean[] dateTimeWithTimezone,
      int[] sqlTypeHintIds,
      boolean useSqlTypeHintsForNullValues,
      boolean supportsSeparateNaN,
      boolean supportsInfinity,
      int numRows) {
    try {
      batchInsertImpl(
          connection,
          insertTemplate,
          jdbcValueSetter,
          storages,
          batchSize,
          dateTimeWithTimezone,
          sqlTypeHintIds,
          useSqlTypeHintsForNullValues,
          supportsSeparateNaN,
          supportsInfinity,
          numRows);
      return Value.asValue(null);
    } catch (Exception e) {
      return wrapException(e, insertTemplate);
    }
  }

  private static void batchInsertImpl(
      Connection connection,
      String insertTemplate,
      JDBCValueSetter jdbcValueSetter,
      ColumnStorage<?>[] storages,
      int batchSize,
      boolean[] dateTimeWithTimezone,
      int[] sqlTypeHintIds,
      boolean useSqlTypeHintsForNullValues,
      boolean supportsSeparateNaN,
      boolean supportsInfinity,
      int numRows)
      throws SQLException, IllegalStateException {
    try (PreparedStatement stmt = connection.prepareStatement(insertTemplate)) {
      int columnCount = storages.length;

      // Localize storages to avoid issues with foreign memory access.
      var localisedStorages =
          stream(storages).map(Builder::makeLocal).toArray(ColumnStorage<?>[]::new);

      for (int rowId = 0; rowId < numRows; rowId++) {
        for (int columnId = 0; columnId < columnCount; columnId++) {
          ColumnStorage<?> columnStorage = localisedStorages[columnId];
          boolean keepTimezone = dateTimeWithTimezone[columnId];
          int nullType = useSqlTypeHintsForNullValues ? sqlTypeHintIds[columnId] : Types.NULL;
          var value = columnStorage.getItemBoxed(rowId);
          setStatementValue(
              stmt,
              columnId + 1,
              value,
              jdbcValueSetter,
              keepTimezone,
              nullType,
              supportsSeparateNaN,
              supportsInfinity);
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
            "A single update within the batch unexpectedly affected " + affectedRows + " rows.");
      }
    }
  }

  private static void setStatementValue(
      PreparedStatement stmt,
      int columnIndex,
      Object value,
      JDBCValueSetter jdbcValueSetter,
      boolean dateTimeWithTimezone,
      int nullType,
      boolean supportsSeparateNaN,
      boolean supportsInfinity)
      throws SQLException {
    switch (value) {
      case null -> stmt.setNull(columnIndex, nullType);
      case Boolean boolValue -> stmt.setBoolean(columnIndex, boolValue);
      case Byte byteValue -> stmt.setLong(columnIndex, byteValue.longValue());
      case Short shortValue -> stmt.setLong(columnIndex, shortValue.longValue());
      case Integer integerValue -> stmt.setLong(columnIndex, integerValue.longValue());
      case Long longValue -> stmt.setLong(columnIndex, longValue);
      case BigInteger bigIntegerValue ->
          jdbcValueSetter.setBigDecimal(stmt, columnIndex, bigIntegerValue, 0);
      case Float floatValue ->
          setFloatingPointValue(
              stmt, columnIndex, floatValue, supportsSeparateNaN, supportsInfinity);
      case Double doubleValue ->
          setFloatingPointValue(
              stmt, columnIndex, doubleValue, supportsSeparateNaN, supportsInfinity);
      case BigDecimal bigDecimalValue -> stmt.setBigDecimal(columnIndex, bigDecimalValue);
      case String textValue -> stmt.setString(columnIndex, textValue);
      case ZonedDateTime zonedDateTime ->
          setDateTimeValue(stmt, columnIndex, zonedDateTime, jdbcValueSetter, dateTimeWithTimezone);
      case LocalTime localTime -> jdbcValueSetter.setLocalTime(stmt, columnIndex, localTime);
      case LocalDate localDate -> jdbcValueSetter.setLocalDate(stmt, columnIndex, localDate);
      default -> stmt.setObject(columnIndex, value);
    }
  }

  private static void setFloatingPointValue(
      PreparedStatement stmt,
      int columnIndex,
      double value,
      boolean supportsSeparateNaN,
      boolean supportsInfinity)
      throws SQLException {
    if ((Double.isNaN(value) && !supportsSeparateNaN)
        || (Double.isInfinite(value) && !supportsInfinity)) {
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

  private static Value wrapException(Exception e, String insertTemplate) {
    if (e instanceof SQLTimeoutException timeoutException) {
      return EnsoMeta.asDataflowError(
          EnsoMeta.makeInstance(
              "Standard.Database.Errors",
              "SQL_Timeout",
              "Error",
              timeoutException,
              insertTemplate));
    }

    if (e instanceof SQLException sqlException) {
      return EnsoMeta.asDataflowError(
          EnsoMeta.makeInstance(
              "Standard.Database.Errors", "SQL_Error", "Error", sqlException, insertTemplate));
    }

    var ensoAtom = EnsoExceptionWrapper.wrapCommonExceptions(e);
    if (ensoAtom.isEmpty()) {
      if (e instanceof RuntimeException runtimeException) {
        throw runtimeException;
      }
      throw new RuntimeException(e);
    }
    return EnsoMeta.asDataflowError(ensoAtom.get());
  }
}
