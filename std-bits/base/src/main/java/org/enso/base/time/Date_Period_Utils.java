package org.enso.base.time;

import java.time.DayOfWeek;
import java.time.YearMonth;
import java.time.temporal.*;

public class Date_Period_Utils implements TimeUtilsBase {

  public static TemporalAdjuster quarter_start =
      (Temporal temporal) -> {
        int currentQuarter = temporal.get(IsoFields.QUARTER_OF_YEAR);
        int month = (currentQuarter - 1) * 3 + 1;
        return temporal
            .with(ChronoField.MONTH_OF_YEAR, month)
            .with(TemporalAdjusters.firstDayOfMonth());
      };

  public static TemporalAdjuster quarter_end =
      (Temporal temporal) -> {
        int currentQuarter = YearMonth.from(temporal).get(IsoFields.QUARTER_OF_YEAR);
        int month = (currentQuarter - 1) * 3 + 3;
        return temporal
            .with(ChronoField.MONTH_OF_YEAR, month)
            .with(TemporalAdjusters.lastDayOfMonth());
      };

  public static TemporalAdjuster end_of_week(DayOfWeek start_of_week) {
      return (Temporal temporal) -> temporal.with(TemporalAdjusters.next(start_of_week)).minus(1, ChronoUnit.DAYS);
  }
}
