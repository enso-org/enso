package org.enso.base.time;

import java.time.ZonedDateTime;
import java.time.temporal.TemporalAdjuster;
import java.time.temporal.TemporalUnit;

public class Date_Time_Utils implements TimeUtilsBase {
  public static final Date_Time_Utils INSTANCE = new Date_Time_Utils();

  public ZonedDateTime start_of_time_period(ZonedDateTime date, TemporalUnit unit) {
    return date.truncatedTo(unit);
  }

  public ZonedDateTime end_of_time_period(ZonedDateTime date, TemporalUnit unit) {
    return date.truncatedTo(unit).plus(1, unit).minusNanos(1);
  }

  public ZonedDateTime apply_adjuster(ZonedDateTime date, TemporalAdjuster adjuster) {
    return date.with(adjuster);
  }
}
