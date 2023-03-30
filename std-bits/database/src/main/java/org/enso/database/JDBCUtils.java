package org.enso.database;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;

public class JDBCUtils {
  /**
   * Converts a ZonedDateTime to a Timestamp (note loses the timezone).
   *
   * @param zonedDateTime the ZonedDateTime to convert
   * @return the converted Timestamp
   */
  public static Timestamp getTimestamp(ZonedDateTime zonedDateTime) {
    return Timestamp.from(zonedDateTime.toInstant());
  }

  public static ZonedDateTime getZonedDateTime(ResultSet rs, int columnIndex) throws SQLException {
    OffsetDateTime offsetDateTime = rs.getObject(columnIndex, OffsetDateTime.class);
    return offsetDateTime.toZonedDateTime();
  }
}
