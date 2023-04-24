package org.enso.database;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;

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

  /** Sets a ZonedDateTime in a PreparedStatement. */
  public static void setZonedDateTime(
      PreparedStatement stmt, int columnIndex, ZonedDateTime zonedDateTime) throws SQLException {
    stmt.setObject(columnIndex, zonedDateTime.toOffsetDateTime(), Types.TIMESTAMP_WITH_TIMEZONE);
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
