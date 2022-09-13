package org.enso.base.time;

import java.time.LocalDate;
import java.time.temporal.TemporalAdjuster;

public class Date_Utils implements TimeUtilsBase {
  public static final Date_Utils INSTANCE = new Date_Utils();

  public LocalDate apply_adjuster(LocalDate date, TemporalAdjuster adjuster) {
    return date.with(adjuster);
  }
}
