package org.enso.database;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
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
}
