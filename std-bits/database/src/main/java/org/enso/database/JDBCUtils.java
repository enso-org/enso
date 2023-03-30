package org.enso.database;

import org.postgresql.PGStatement;
import org.postgresql.core.Provider;
import org.postgresql.jdbc.PgStatement;
import org.postgresql.jdbc.TimestampUtils;

import java.sql.*;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.TimeZone;

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

  // TODO remove?
  private static TimestampUtils PG_TIMESTAMP_UTILS = null;
  private static TimestampUtils getPostgresTimestampUtils() {
    if (PG_TIMESTAMP_UTILS == null) {
      // We don't know what is the true setting for the given connection.
      // But this setting is only relevant for binary helpers, so we just don't use those.
      boolean usesDouble = true;
      // Likewise, we just assume the timezone will not be used.
      Provider<TimeZone> timeZoneProvider = () -> null;
      PG_TIMESTAMP_UTILS = new TimestampUtils(usesDouble, timeZoneProvider);
    }
    return PG_TIMESTAMP_UTILS;
  }
}
