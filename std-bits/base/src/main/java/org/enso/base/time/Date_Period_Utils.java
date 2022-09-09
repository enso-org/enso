package org.enso.base.time;

import java.time.temporal.ChronoField;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAdjusters;

public class Date_Period_Utils implements TimeUtilsBase {
  public static Temporal quarter_start(Temporal temporal) {
    int month = temporal.get(ChronoField.MONTH_OF_YEAR);
    int quarter = (month - 1) / 3;
    int firstMonth = quarter * 3 + 1;
    return temporal
        .with(ChronoField.MONTH_OF_YEAR, firstMonth)
        .with(TemporalAdjusters.firstDayOfMonth());
  }

  public static Temporal quarter_end(Temporal temporal) {
    int month = temporal.get(ChronoField.MONTH_OF_YEAR);
    int quarter = (month - 1) / 3;
    int lastMonth = quarter * 3 + 3;
    return temporal
        .with(ChronoField.MONTH_OF_YEAR, lastMonth)
        .with(TemporalAdjusters.lastDayOfMonth());
  }
}
