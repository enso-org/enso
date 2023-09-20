package org.enso.base.time;

import org.enso.polyglot.common_utils.Core_Date_Utils;
import org.graalvm.collections.Pair;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalQueries;

import static java.time.temporal.ChronoField.INSTANT_SECONDS;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;

public class EnsoDateTimeFormatter {
  private final DateTimeFormatter formatter;
  private final Pair<Character, String> isoReplacementPair;
  private final String originalPattern;
  private final FormatterKind formatterKind;

  private EnsoDateTimeFormatter(DateTimeFormatter formatter, Pair<Character, String> isoReplacementPair, String originalPattern, FormatterKind formatterKind) {
    this.formatter = formatter;
    this.isoReplacementPair = isoReplacementPair;
    this.originalPattern = originalPattern;
    this.formatterKind = formatterKind;
  }

  public EnsoDateTimeFormatter(DateTimeFormatter formatter, String originalPattern, FormatterKind formatterKind) {
    this(formatter, null, originalPattern, formatterKind);
  }

  public static EnsoDateTimeFormatter makeISOConstant(DateTimeFormatter formatter, String name) {
    return new EnsoDateTimeFormatter(formatter, Pair.create(' ', "T"), name, FormatterKind.CONSTANT);
  }

  public static EnsoDateTimeFormatter default_enso_zoned_date_time_formatter() {
    return new EnsoDateTimeFormatter(
        Core_Date_Utils.defaultZonedDateTimeFormatter(),
        Pair.create('T', " "),
        "default_enso_zoned_date_time",
        FormatterKind.CONSTANT
    );
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

  private String normaliseInput(String dateString) {
    if (isoReplacementPair == null) {
      // Nothing to do
      return dateString;
    }

    char from = isoReplacementPair.getLeft();
    String to = isoReplacementPair.getRight();

    if (dateString != null && dateString.length() > 10 && dateString.charAt(10) == from) {
      var builder = new StringBuilder(dateString);
      builder.replace(10, 11, to);
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
    dateString = normaliseInput(dateString);

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
    dateString = normaliseInput(dateString);

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
    text = normaliseInput(text);
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
