package org.enso.snowflake;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

public class SnowflakeJDBCUtils {
  private static final DateTimeFormatter dateTimeWithOffsetFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSSSS XXX");

  public static void setDateTime(
      PreparedStatement stmt, int columnIndex, ZonedDateTime dateTime, boolean keepOffset)
      throws SQLException {
    if (keepOffset) {
      String formatted = dateTime.format(dateTimeWithOffsetFormatter);
      stmt.setString(columnIndex, formatted);
    } else {
      LocalDateTime localDateTime = dateTime.toLocalDateTime();
      stmt.setString(columnIndex, localDateTime.toString());
    }
  }

  public static void setTimeOfDay(PreparedStatement stmt, int columnIndex, LocalTime timeOfDay)
      throws SQLException {
    // We use setString instead of setTime, because setTime was losing milliseconds,
    // or with some tricks maybe could have milliseconds but not nanoseconds.
    // With setting as text we can keep the precision.
    stmt.setString(columnIndex, timeOfDay.toString());
  }

  public static void setDate(PreparedStatement stmt, int columnIndex, LocalDate date)
      throws SQLException {
    stmt.setDate(columnIndex, java.sql.Date.valueOf(date));
  }
}
