package org.enso.snowflake;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import org.enso.database.JDBCValueSetter;

public final class SnowflakeJDBCValueSetter extends JDBCValueSetter {
  private static final DateTimeFormatter dateTimeWithOffsetFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSS XXX");

  public SnowflakeJDBCValueSetter() {
    super("Snowflake");
  }

  public void setZonedDateTime(PreparedStatement stmt, int columnIndex, ZonedDateTime dateTime)
      throws SQLException {
    String formatted = dateTime.format(dateTimeWithOffsetFormatter);
    stmt.setString(columnIndex, formatted);
  }

  /** Sets a ZonedDateTime converting it to LocalDateTime in a PreparedStatement. */
  public void setLocalDateTime(PreparedStatement stmt, int columnIndex, ZonedDateTime zonedDateTime)
      throws SQLException {
    LocalDateTime localDateTime = zonedDateTime.toLocalDateTime();
    stmt.setString(columnIndex, localDateTime.toString());
  }

  public void setLocalTime(PreparedStatement stmt, int columnIndex, LocalTime timeOfDay)
      throws SQLException {
    // We use setString instead of setTime, because setTime was losing milliseconds,
    // or with some tricks maybe could have milliseconds but not nanoseconds.
    // With setting as text we can keep the precision.
    stmt.setString(columnIndex, timeOfDay.toString());
  }

  public void setLocalDate(PreparedStatement stmt, int columnIndex, LocalDate date)
      throws SQLException {
    stmt.setDate(columnIndex, java.sql.Date.valueOf(date));
  }
}
