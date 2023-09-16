package org.enso.base.time;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalQueries;

import static java.time.temporal.ChronoField.INSTANT_SECONDS;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;

public class EnsoDateTimeFormatter {
  private final DateTimeFormatter formatter;
  private final boolean needsISOTreplaceWorkaround;
  private final String originalPattern;
  private final FormatterKind formatterKind;

  public EnsoDateTimeFormatter(DateTimeFormatter formatter, boolean needsISOreplaceTWorkaround, String originalPattern, FormatterKind formatterKind) {
    this.formatter = formatter;
    this.needsISOTreplaceWorkaround = needsISOreplaceTWorkaround;
    this.originalPattern = originalPattern;
    this.formatterKind = formatterKind;
  }

  public DateTimeFormatter getRawJavaFormatter() {
    return formatter;
  }

  public String getOriginalPattern() {
    return originalPattern;
  }

  public FormatterKind getFormatterKind() {
    return formatterKind;
  }

  private String normaliseISODateTime(String dateString) {
    if (dateString != null && dateString.length() > 10 && dateString.charAt(10) == ' ') {
      var builder = new StringBuilder(dateString);
      builder.replace(10, 11, "T");
      return builder.toString();
    }

    return dateString;
  }

  @Override
  public String toString() {
    return switch (formatterKind) {
      case SIMPLE -> "(Simple) " + originalPattern;
      case ISO_WEEK_DATE -> "(ISO Week Date Format) " + originalPattern;
      case RAW_JAVA -> "(Java Format Pattern) " + originalPattern;
      case CONSTANT -> originalPattern;
    };
  }

  public LocalDate parseLocalDate(String dateString) {
    if (needsISOTreplaceWorkaround) {
      dateString = normaliseISODateTime(dateString);
    }

    var parsed = formatter.parse(dateString);

    if (parsed.isSupported(ChronoField.EPOCH_DAY)) {
      return LocalDate.ofEpochDay(parsed.getLong(ChronoField.EPOCH_DAY));
    }

    // Allow Year and Month to be parsed without a day (use first day of month).
    if (parsed.isSupported(ChronoField.YEAR) && parsed.isSupported(ChronoField.MONTH_OF_YEAR)) {
      var dayOfMonth =
          parsed.isSupported(ChronoField.DAY_OF_MONTH) ? parsed.get(ChronoField.DAY_OF_MONTH) : 1;
      return LocalDate.of(
          parsed.get(ChronoField.YEAR), parsed.get(ChronoField.MONTH_OF_YEAR), dayOfMonth);
    }

    // Allow Month and Day to be parsed without a year (use current year).
    if (parsed.isSupported(ChronoField.DAY_OF_MONTH)
        && parsed.isSupported(ChronoField.MONTH_OF_YEAR)) {
      return LocalDate.of(
          LocalDate.now().getYear(),
          parsed.get(ChronoField.MONTH_OF_YEAR),
          parsed.get(ChronoField.DAY_OF_MONTH));
    }

    // This will usually throw at this point, but it will construct a more informative exception than we could.
    return LocalDate.from(parsed);
  }

  public ZonedDateTime parseZonedDateTime(String dateString) {
    if (needsISOTreplaceWorkaround) {
      dateString = normaliseISODateTime(dateString);
    }

    var resolved = formatter.parse(dateString);

    try {
      // Resolve Zone
      var zone = resolved.query(TemporalQueries.zoneId());
      zone =
          zone != null
              ? zone
              : (resolved.isSupported(ChronoField.OFFSET_SECONDS)
              ? ZoneOffset.ofTotalSeconds(resolved.get(ChronoField.OFFSET_SECONDS))
              : ZoneId.systemDefault());

      // Instant Based
      if (resolved.isSupported(INSTANT_SECONDS)) {
        long epochSecond = resolved.getLong(INSTANT_SECONDS);
        int nanoOfSecond = resolved.get(NANO_OF_SECOND);
        return ZonedDateTime.ofInstant(
            java.time.Instant.ofEpochSecond(epochSecond, nanoOfSecond), zone);
      }

      // Local Based
      var localDate = LocalDate.from(resolved);
      var localTime = LocalTime.from(resolved);
      return ZonedDateTime.of(localDate, localTime, zone);
    } catch (DateTimeException e) {
      throw new DateTimeException("Unable to parse Text '" + dateString + "' to Date_Time: "+e.getMessage(), e);
    } catch (ArithmeticException e) {
      throw new DateTimeException(
          "Unable to parse Text '" + dateString + "' to Date_Time due to arithmetic error.", e);
    }
  }

  public LocalTime parseLocalTime(String text) {
    if (needsISOTreplaceWorkaround) {
      text = normaliseISODateTime(text);
    }

    return LocalTime.parse(text, formatter);
  }

  public String formatLocalDate(LocalDate date) {
    return formatter.format(date);
  }

  public String formatZonedDateTime(ZonedDateTime dateTime) {
    return formatter.format(dateTime);
  }

  public String formatLocalTime(LocalTime time) {
    return formatter.format(time);
  }
}
