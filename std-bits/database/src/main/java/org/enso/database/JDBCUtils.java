package org.enso.database;

import java.sql.Timestamp;
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
}
