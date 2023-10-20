package org.enso.database;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import org.enso.polyglot.common_utils.Core_Date_Utils;

public class JDBCUtils {

  /**
   * Gets a ZonedDateTime from a ResultSet.
   *
   * <p>Note that the only timezone information is based on the offset provided by the database, so
   * only simple offset timezones will be returned. No support for named timezones.
   */
  public static ZonedDateTime getZonedDateTime(ResultSet rs, int columnIndex) throws SQLException {
    OffsetDateTime offsetDateTime = rs.getObject(columnIndex, OffsetDateTime.class);
    if (offsetDateTime == null) {
      return null;
    }
    return offsetDateTime.toZonedDateTime();
  }

  /**
   * Gets a ZonedDateTime from a ResultSet, interpreting the result as LocalDateTime and then adding
   * the system default timezone.
   */
  public static ZonedDateTime getLocalDateTimeAsZoned(ResultSet rs, int columnIndex)
      throws SQLException {
    LocalDateTime localDateTime = rs.getObject(columnIndex, LocalDateTime.class);
    if (localDateTime == null) {
      return null;
    }
    return localDateTime.atZone(Core_Date_Utils.defaultSystemZone());
  }

  /** Sets a ZonedDateTime in a PreparedStatement. */
  public static void setZonedDateTime(
      PreparedStatement stmt, int columnIndex, ZonedDateTime zonedDateTime) throws SQLException {
    stmt.setObject(columnIndex, zonedDateTime.toOffsetDateTime(), Types.TIMESTAMP_WITH_TIMEZONE);
  }

  /** Sets a ZonedDateTime converting it to LocalDateTime in a PreparedStatement. */
  public static void setLocalDateTime(
      PreparedStatement stmt, int columnIndex, ZonedDateTime zonedDateTime) throws SQLException {
    LocalDateTime localDateTime = zonedDateTime.toLocalDateTime();
    stmt.setObject(columnIndex, localDateTime, Types.TIMESTAMP);
  }

  /** Sets a LocalTime in a PreparedStatement. */
  public static void setLocalTime(PreparedStatement stmt, int columnIndex, LocalTime localTime)
      throws SQLException {
    stmt.setObject(columnIndex, localTime, Types.TIME);
  }

  /** Sets a LocalDate in a PreparedStatement. */
  public static void setLocalDate(PreparedStatement stmt, int columnIndex, LocalDate localDate)
      throws SQLException {
    stmt.setObject(columnIndex, localDate, Types.DATE);
  }
}
