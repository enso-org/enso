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
import java.time.format.ResolverStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalField;
import java.time.temporal.TemporalQueries;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import static java.time.temporal.ChronoField.INSTANT_SECONDS;
import static java.time.temporal.ChronoField.NANO_OF_SECOND;

/**
 * An Enso representation of the DateTimeFormatter.
 * <p>
 * It adds some additional functionality to the Java formatter - including a workaround for making the `T` in ISO dates
 * optional and tracking how it was constructed.
 */
public class EnsoDateTimeFormatter {
  private final DateTimeFormatter formatter;
  private final Pair<Character, String> isoReplacementPair;
  private final String originalPattern;
  private final FormatterKind formatterKind;

  private EnsoDateTimeFormatter(DateTimeFormatter formatter, Pair<Character, String> isoReplacementPair,
                                String originalPattern, FormatterKind formatterKind) {
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
        Core_Date_Utils.defaultZonedDateTimeFormatter,
        Pair.create('T', " "),
        "default_enso_zoned_date_time",
        FormatterKind.CONSTANT
    );
  }

  public EnsoDateTimeFormatter withLocale(Locale locale) {
    return new EnsoDateTimeFormatter(formatter.withLocale(locale), isoReplacementPair, originalPattern, formatterKind);
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
      case SIMPLE -> originalPattern;
      case ISO_WEEK_DATE -> "(ISO Week Date Format) " + originalPattern;
      case RAW_JAVA -> "(Java DateTimeFormatter) " + (originalPattern != null ? originalPattern : formatter.toString());
      case CONSTANT -> originalPattern;
    };
  }

  public LocalDate parseLocalDate(String dateString) {
    dateString = normaliseInput(dateString);
    return LocalDate.parse(dateString, formatter);
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
      throw new DateTimeException("Unable to parse Text '" + dateString + "' to Date_Time: " + e.getMessage(), e);
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

  @Override
  public int hashCode() {
    // We ignore formatter here because it has identity semantics.
    return Arrays.hashCode(new Object[]{isoReplacementPair, originalPattern, formatterKind});
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof EnsoDateTimeFormatter other) {
      // The DateTimeFormatter has identity semantics, so instead we try to check the pattern instead, if available.
      if (originalPattern != null) {
        return formatterKind == other.formatterKind && originalPattern.equals(other.originalPattern) && isoReplacementPair.equals(other.isoReplacementPair) && formatter.getLocale().equals(other.formatter.getLocale());
      } else {
        return formatterKind == other.formatterKind && formatter.equals(other.formatter);
      }
    } else {
      return false;
    }
  }
}
