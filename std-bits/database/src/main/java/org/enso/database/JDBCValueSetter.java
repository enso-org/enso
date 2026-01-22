package org.enso.database;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.base.polyglot.NumericConverter;
import org.graalvm.polyglot.Value;

public class JDBCValueSetter {
  private final String databaseName;

  /**
   * A helper method that creates a JDBCDriverTypes record.
   *
   * @param databaseName the name of the Database type for the record
   * @return a new JDBCDriverTypes record
   */
  public static JDBCValueSetter create(String databaseName) {
    return new JDBCValueSetter(databaseName);
  }

  protected JDBCValueSetter(String databaseName) {
    this.databaseName = databaseName;
  }

  public String databaseName() {
    return databaseName;
  }

  /** Sets an Enso Integer in a PreparedStatement. */
  public void setInteger(PreparedStatement stmt, int columnIndex, Value value) throws SQLException {
    if (NumericConverter.isBigInteger(value)) {
      var bigDecimal = NumericConverter.bigIntegerAsBigDecimal(value.asBigInteger());
      stmt.setBigDecimal(columnIndex, bigDecimal);
    } else {
      stmt.setLong(columnIndex, value.asLong());
    }
  }

  /** Sets a ZonedDateTime in a PreparedStatement. */
  public void setZonedDateTime(PreparedStatement stmt, int columnIndex, ZonedDateTime zonedDateTime)
      throws SQLException {
    stmt.setObject(columnIndex, zonedDateTime.toOffsetDateTime(), Types.TIMESTAMP_WITH_TIMEZONE);
  }

  /** Sets a ZonedDateTime converting it to LocalDateTime in a PreparedStatement. */
  public void setLocalDateTime(PreparedStatement stmt, int columnIndex, ZonedDateTime zonedDateTime)
      throws SQLException {
    LocalDateTime localDateTime = zonedDateTime.toLocalDateTime();
    stmt.setObject(columnIndex, localDateTime, Types.TIMESTAMP);
  }

  /** Sets a LocalTime in a PreparedStatement. */
  public void setLocalTime(PreparedStatement stmt, int columnIndex, LocalTime localTime)
      throws SQLException {
    stmt.setObject(columnIndex, localTime, Types.TIME);
  }

  /** Sets a LocalDate in a PreparedStatement. */
  public void setLocalDate(PreparedStatement stmt, int columnIndex, LocalDate localDate)
      throws SQLException {
    stmt.setObject(columnIndex, localDate, Types.DATE);
  }

  public void setBigDecimal(
      PreparedStatement stmt, int columnIndex, BigInteger unscaledValue, int scale)
      throws SQLException {
    var big = new BigDecimal(unscaledValue, scale);
    stmt.setBigDecimal(columnIndex, big);
  }
}
