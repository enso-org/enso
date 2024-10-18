package org.enso.table.excel;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;

public class ExcelUtils {
  private static final LocalDate EPOCH_1900 = LocalDate.of(1900, 1, 1);
  private static final long MILLIS_PER_DAY = 24*60*60*1000L;

  /** Converts an Excel date-time value to a {@link Temporal}. */
  public static Temporal fromExcelDateTime(double value) {
    // Excel treats 1900-02-29 as a valid date, which it is not a valid date.
    if (value >= 60 && value < 61) {
      return null;
    }

    // For days before 1900-01-01, Stored as milliseconds before 1900-01-01.
    long days = (long)value;

    // Extract the milliseconds part of the value.
    long millis = (long)((value - days) * MILLIS_PER_DAY + 0.5);
    if (millis < 0) {
      millis += MILLIS_PER_DAY;
    }

    // Excel stores times as 0 to 1.
    if (days == 0) {
      return LocalTime.ofNanoOfDay(millis * 1000000);
    }

    int shift = days > 0 && days < 60 ? -1 : 0;
    LocalDate date = EPOCH_1900.plusDays(days + shift);

    return millis == 0 ? date : date.atTime(LocalTime.ofNanoOfDay(millis * 1000000));
  }

  /** Converts a {@link Temporal} to an Excel date-time value. */
  public static double toExcelDateTime(Temporal temporal) {
    return switch (temporal) {
      case ZonedDateTime zonedDateTime -> toExcelDateTime(zonedDateTime.toLocalDateTime());
      case LocalDateTime dateTime -> toExcelDateTime(dateTime.toLocalDate()) + toExcelDateTime(dateTime.toLocalTime());
      case LocalDate date -> {
        long days = ChronoUnit.DAYS.between(EPOCH_1900, date);

        // If the Date is between 1900-01-01 and 1900-02-28, EPOCH needs to be 1 day earlier.
        if (date.getYear() == 1900 && date.getMonthValue() < 3) {
          days--;
        }

        yield days;
      }
      case LocalTime time -> time.toNanoOfDay() / 1000000.0 / MILLIS_PER_DAY;
      default -> throw new IllegalArgumentException("Unsupported Temporal type: " + temporal.getClass());
    };
  }
}
