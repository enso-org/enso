package org.enso.base.time;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

/**
 * Enso abstraction over the {@link DateTimeFormatter}.
 *
 * <p>It adds some additional functionality to the Java formatter - including a workaround for
 * making the `T` in ISO dates optional and tracking how it was constructed.
 */
public interface EnsoDateTimeFormatter {
  public String describe();

  public LocalDate parseLocalDate(String dateString);

  public ZonedDateTime parseZonedDateTime(String dateString);

  public LocalTime parseLocalTime(String text);

  public String formatLocalDate(LocalDate date);

  public String formatZonedDateTime(ZonedDateTime dateTime);

  public String formatLocalTime(LocalTime time);

  public String formatLocalDateTime(LocalDateTime dateTime);

  //
  // factory methods
  //

  public static EnsoDateTimeFormatter create(DateTimeFormatter f, String p, FormatterKind k) {
    return new EnsoDateTimeFormatterImpl(f, p, k);
  }

  public static EnsoDateTimeFormatter default_enso_zoned_date_time_formatter() {
    return EnsoDateTimeFormatterImpl.default_enso_zoned_date_time_formatter();
  }

  public static EnsoDateTimeFormatter makeISOConstant(DateTimeFormatter formatter, String name) {
    return EnsoDateTimeFormatterImpl.makeISOConstant(formatter, name);
  }

  public EnsoDateTimeFormatter withLocale(Locale locale);
}
