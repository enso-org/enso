package org.enso.table.excel;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

public class ExcelUtils {
  // The epoch for Excel date-time values. Due to 1900-02-29 being a valid date
  // in Excel, it is actually 1899-12-30. Excel dates are counted from 1 being
  // 1900-01-01.
  private static final LocalDate EPOCH_1900 = LocalDate.of(1899, 12, 30);
  // The epoch for Excel date-time values in 1904 format. Less anomalies in this
  // mode. Times and 1-Jan-1904 are the same.
  private static final LocalDate EPOCH_1904 = LocalDate.of(1904, 1, 1);
  private static final long MILLIS_PER_DAY = 24 * 60 * 60 * 1000L;

  public static boolean is1904DateSystem(Workbook workbook) {
    boolean use1904Dates = false;
    if (workbook instanceof XSSFWorkbook xssfWorkbook) {
      use1904Dates = xssfWorkbook.isDate1904();
    }
    return use1904Dates;
  }

  /** Converts an Excel date-time value to a {@link Temporal}. */
  public static Temporal fromExcelDateTime(double value) {
    // Excel treats 1900-02-29 as a valid date, which it is not a valid date.
    if (value >= 60 && value < 61) {
      return null;
    }

    // For days before 1900-01-01, Stored as milliseconds before 1900-01-01.
    long days = (long) value;

    // Extract the milliseconds part of the value.
    long millis = (long) ((value - days) * MILLIS_PER_DAY + (value < 0 ? -0.5 : 0.5));
    if (millis < 0) {
      millis += MILLIS_PER_DAY;
    }
    if (millis != 0 && value < 0) {
      days--;
    }

    // Excel stores times as 0 to 1.
    if (days == 0) {
      return LocalTime.ofNanoOfDay(millis * 1000000);
    }

    int shift = 0;
    if (days > 0 && days < 60) {
      // Due to a bug in Excel, 1900-02-29 is treated as a valid date.
      // So within the first two months of 1900, the epoch needs to be 1 day later.
      shift = 1;
    } else if (days < 0) {
      // For days before 1900-01-01, Excel has no representation.
      // 0 is 1900-01-00 in Excel.
      // We make -1 as 1899-12-31, -2 as 1899-12-30, etc.
      // This needs the shift to be 2 days later.
      shift = 2;
    }
    LocalDate date = EPOCH_1900.plusDays(days + shift);

    return millis < 1000
        ? date
        : date.atTime(LocalTime.ofNanoOfDay(millis * 1000000)).atZone(ZoneId.systemDefault());
  }

  /**
   * Converts an Excel date-time value to a {@link Temporal} in 1904 format. Note: Times returned as
   * 1-Jan-1904 date times.
   */
  public static Temporal fromExcelDateTime1904(double value) {
    // For days before 1904-01-01, Stored as milliseconds before 1904-01-01.
    long days = (long) value;

    // Extract the milliseconds part of the value.
    long millis = (long) ((value - days) * MILLIS_PER_DAY + (value < 0 ? -0.5 : 0.5));
    if (millis < 0) {
      millis += MILLIS_PER_DAY;
    }
    if (millis != 0 && value < 0) {
      days--;
    }

    LocalDate date = EPOCH_1904.plusDays(days);
    return millis < 1000
        ? date
        : date.atTime(LocalTime.ofNanoOfDay(millis * 1000000)).atZone(ZoneId.systemDefault());
  }

  /** Converts a {@link Temporal} to an Excel date-time value. */
  public static double toExcelDateTime(Temporal temporal) {
    return switch (temporal) {
      case ZonedDateTime zonedDateTime -> toExcelDateTime(zonedDateTime.toLocalDateTime());
      case LocalDateTime dateTime -> toExcelDateTime(dateTime.toLocalDate())
          + toExcelDateTime(dateTime.toLocalTime());
      case LocalDate date -> {
        long days = ChronoUnit.DAYS.between(EPOCH_1900, date);

        if (date.getYear() == 1900 && date.getMonthValue() < 3) {
          // Due to a bug in Excel, 1900-02-29 is treated as a valid date.
          // So within the first two months of 1900, the epoch needs to be 1 day later.
          days--;
        }
        if (date.getYear() < 1900) {
          // For days before 1900-01-01, Excel has no representation.
          // 0 is 1900-01-00 in Excel.
          // We make -1 as 1899-12-31, -2 as 1899-12-30, etc.
          // This means the epoch needs to be 2 days later.
          days -= 2;
        }

        yield days;
      }
      case LocalTime time -> time.toNanoOfDay() / 1000000.0 / MILLIS_PER_DAY;
      default -> throw new IllegalArgumentException(
          "Unsupported Temporal type: " + temporal.getClass());
    };
  }

  /** Converts a {@link Temporal} to an Excel date-time value using 1904 system. */
  public static double toExcelDateTime1904(Temporal temporal) {
    return switch (temporal) {
      case ZonedDateTime zonedDateTime -> toExcelDateTime1904(zonedDateTime.toLocalDateTime());
      case LocalDateTime dateTime -> toExcelDateTime1904(dateTime.toLocalDate())
          + toExcelDateTime1904(dateTime.toLocalTime());
      case LocalDate date -> ChronoUnit.DAYS.between(EPOCH_1904, date);
      case LocalTime time -> time.toNanoOfDay() / 1000000.0 / MILLIS_PER_DAY;
      default -> throw new IllegalArgumentException(
          "Unsupported Temporal type: " + temporal.getClass());
    };
  }
}
