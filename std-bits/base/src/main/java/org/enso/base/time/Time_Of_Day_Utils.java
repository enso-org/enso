package org.enso.base.time;

import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalUnit;

public class Time_Of_Day_Utils implements TimeUtilsBase {
  public static final Time_Of_Day_Utils INSTANCE = new Time_Of_Day_Utils();

  public LocalTime start_of_time_period(LocalTime date, TemporalUnit unit) {
    return date.truncatedTo(unit);
  }

  public LocalTime end_of_time_period(LocalTime date, TemporalUnit unit) {
    LocalTime truncated = date.truncatedTo(unit);
    LocalTime adjusted = unit.equals(ChronoUnit.DAYS) ? truncated : truncated.plus(1, unit);
    return adjusted.minusNanos(1);
  }
}
