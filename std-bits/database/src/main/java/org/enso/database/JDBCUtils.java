package org.enso.database;

import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import org.enso.polyglot.common_utils.Core_Date_Utils;

public class JDBCUtils {
  /** Reads all values from a column in a ResultSet into a String array. */
  public static String[] readTextColumn(ResultSet rs, String name) throws SQLException {
    if (rs.isClosed()) {
      return new String[0];
    }

    int index = rs.findColumn(name);

    var results = new ArrayList<String>();
    while (rs.next()) {
      var value = rs.getString(index);
      results.add(value);
    }
    return results.toArray(new String[0]);
  }

  /** Gets a LocalDate from a ResultSet. */
  public static LocalDate getLocalDate(ResultSet rs, int columnIndex) throws SQLException {
    var sqlDate = rs.getDate(columnIndex);
    return sqlDate == null ? null : sqlDate.toLocalDate();
  }

  /** Gets a LocalTime from a ResultSet. */
  public static LocalTime getLocalTime(ResultSet rs, int columnIndex) throws SQLException {
    return rs.getObject(columnIndex, LocalTime.class);
  }

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

  /**
   * Gets a BigDecimal from a ResultSet, but handles the case of databases without direct support
   * for BigDecimals, which may return a float or double. In the case of nan / inf values, return
   * null.
   */
  public static BigDecimal getBigDecimalHandleSpecialFloats(ResultSet rs, int columnIndex)
      throws SQLException {
    try {
      return rs.getBigDecimal(columnIndex);
    } catch (SQLException e) {
      try {
        double d = rs.getDouble(columnIndex);
        if (Double.isNaN(d) || Double.isInfinite(d)) {
          return null;
        } else {
          throw e;
        }
      } catch (SQLException eIgnore) {
        throw e;
      }
    }
  }
}
