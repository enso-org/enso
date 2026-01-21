package org.enso.base.time;

import static java.time.temporal.ChronoField.INSTANT_SECONDS;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.time.temporal.TemporalQueries;
import java.util.Arrays;
import java.util.Locale;
import java.util.Objects;
import org.enso.polyglot.common_utils.Core_Date_Utils;

/**
 * An Enso representation of the DateTimeFormatter.
 *
 * <p>It adds some additional functionality to the Java formatter - including a workaround for
 * making the `T` in ISO dates optional and tracking how it was constructed.
 */
final class EnsoDateTimeFormatterImpl implements EnsoDateTimeFormatter {
  private final DateTimeFormatter formatter;
  private final Character isoReplacementKey;
  private final String isoReplacementValue;
  private final String originalPattern;
  private final FormatterKind formatterKind;

  private EnsoDateTimeFormatterImpl(
      DateTimeFormatter formatter,
      Character isoReplacementKey,
      String isoReplacementValue,
      String originalPattern,
      FormatterKind formatterKind) {
    this.formatter = formatter;
    this.isoReplacementKey = isoReplacementKey;
    this.isoReplacementValue = isoReplacementValue;
    this.originalPattern = originalPattern;
    this.formatterKind = formatterKind;
  }

  EnsoDateTimeFormatterImpl(
      DateTimeFormatter formatter, String originalPattern, FormatterKind formatterKind) {
    this(formatter, null, null, originalPattern, formatterKind);
  }

  public static EnsoDateTimeFormatter makeISOConstant(DateTimeFormatter formatter, String name) {
    return new EnsoDateTimeFormatterImpl(formatter, ' ', "T", name, FormatterKind.CONSTANT);
  }

  public static EnsoDateTimeFormatter default_enso_zoned_date_time_formatter() {
    return new EnsoDateTimeFormatterImpl(
        Core_Date_Utils.defaultZonedDateTimeFormatter,
        'T',
        " ",
        "default_enso_zoned_date_time",
        FormatterKind.CONSTANT);
  }

  @Override
  public EnsoDateTimeFormatter withLocale(Locale locale) {
    return new EnsoDateTimeFormatterImpl(
        formatter.withLocale(locale),
        isoReplacementKey,
        isoReplacementValue,
        originalPattern,
        formatterKind);
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
    if (isoReplacementKey == null) {
      // Nothing to do
      return dateString;
    }

    char from = isoReplacementKey;
    String to = isoReplacementValue;

    if (dateString != null && dateString.length() > 10 && dateString.charAt(10) == from) {
      var builder = new StringBuilder(dateString);
      builder.replace(10, 11, to);
      return builder.toString();
    }

    return dateString;
  }

  @Override
  public String describe() {
    return switch (formatterKind) {
      case SIMPLE -> "Date_Time_Formatter.from_simple_pattern " + getOriginalPattern();
      case ISO_WEEK_DATE ->
          "Date_Time_Formatter.from_iso_week_date_pattern " + getOriginalPattern();
      case RAW_JAVA ->
          originalPattern != null
              ? "Date_Time_Formatter.from_java " + getOriginalPattern()
              : "Date_Time_Formatter.from_java " + formatter.toString();
      case CONSTANT -> "Date_Time_Formatter." + getOriginalPattern();
    };
  }

  @Override
  public String toString() {
    return switch (formatterKind) {
      case SIMPLE -> originalPattern;
      case ISO_WEEK_DATE -> "(ISO Week Date Format) " + originalPattern;
      case RAW_JAVA ->
          "(Java DateTimeFormatter) "
              + (originalPattern != null ? originalPattern : formatter.toString());
      case CONSTANT -> originalPattern;
    };
  }

  @Override
  public LocalDate parseLocalDate(String dateString) {
    dateString = normaliseInput(dateString);
    return LocalDate.parse(dateString, formatter);
  }

  @Override
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
      throw new DateTimeException(
          "Unable to parse Text '" + dateString + "' to Date_Time: " + e.getMessage(), e);
    } catch (ArithmeticException e) {
      throw new DateTimeException(
          "Unable to parse Text '" + dateString + "' to Date_Time due to arithmetic error.", e);
    }
  }

  @Override
  public LocalTime parseLocalTime(String text) {
    text = normaliseInput(text);
    return LocalTime.parse(text, formatter);
  }

  @Override
  public String formatLocalDate(LocalDate date) {
    return formatter.format(date);
  }

  @Override
  public String formatZonedDateTime(ZonedDateTime dateTime) {
    return formatter.format(dateTime);
  }

  @Override
  public String formatLocalDateTime(LocalDateTime dateTime) {
    return formatter.format(dateTime);
  }

  @Override
  public String formatLocalTime(LocalTime time) {
    return formatter.format(time);
  }

  @Override
  public int hashCode() {
    // We ignore formatter here because it has identity semantics.
    return Arrays.hashCode(
        new Object[] {isoReplacementKey, isoReplacementValue, originalPattern, formatterKind});
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof EnsoDateTimeFormatterImpl other) {
      // The DateTimeFormatter has identity semantics, so instead we try to check the pattern
      // instead, if available.
      if (originalPattern != null) {
        return formatterKind == other.formatterKind
            && originalPattern.equals(other.originalPattern)
            && Objects.equals(isoReplacementKey, other.isoReplacementKey)
            && Objects.equals(isoReplacementValue, other.isoReplacementValue)
            && formatter.getLocale().equals(other.formatter.getLocale());
      } else {
        return formatterKind == other.formatterKind && formatter.equals(other.formatter);
      }
    } else {
      return false;
    }
  }
}
