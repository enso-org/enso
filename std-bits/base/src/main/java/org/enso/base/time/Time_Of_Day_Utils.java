package org.enso.base.time;

import java.time.LocalTime;
import java.time.temporal.TemporalUnit;

public class Time_Of_Day_Utils implements TimeUtilsBase {
  public static final Time_Of_Day_Utils INSTANCE = new Time_Of_Day_Utils();

  public static LocalTime start_of_time_period(LocalTime date, TemporalUnit unit) {
    return date.truncatedTo(unit);
  }

  public static LocalTime end_of_time_period(LocalTime date, TemporalUnit unit) {
    return date.truncatedTo(unit).plus(1, unit).minusNanos(1);
  }
}
