package org.enso.base.time;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalUnit;

/**
 * Some units that are not available in ChronoUnit but are used by Enso's Date_Period/Time_Period.
 */
public class CustomTemporalUnits {
  /**
   * A unit that represents a 24-hour period.
   *
   * <p>It will behave differently from DAYS if DST is involved in time-supporting Temporal values.
   * However, if a pure-date-based Temporal value is provided (one that does not support time), it
   * will act exactly as DAYS.
   */
  public static final TemporalUnit DAY_AS_24_HOURS = new DayAs24Hours();

  public static final TemporalUnit QUARTERS = new Quarters();

  private static class DayAs24Hours implements TemporalUnit {
    private final Duration duration = Duration.ofHours(24);

    @Override
    public Duration getDuration() {
      return duration;
    }

    @Override
    public boolean isDurationEstimated() {
      return false;
    }

    @Override
    public boolean isDateBased() {
      return true;
    }

    @Override
    public boolean isTimeBased() {
      return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <R extends Temporal> R addTo(R temporal, long amount) {
      if (temporal.isSupported(ChronoUnit.HOURS)) {
        return (R) temporal.plus(amount * 24, ChronoUnit.HOURS);
      } else {
        return (R) temporal.plus(amount, ChronoUnit.DAYS);
      }
    }

    @Override
    public long between(Temporal temporal1Inclusive, Temporal temporal2Exclusive) {
      if (temporal1Inclusive.isSupported(ChronoUnit.HOURS)) {
        return ChronoUnit.HOURS.between(temporal1Inclusive, temporal2Exclusive) / 24;
      } else {
        return ChronoUnit.DAYS.between(temporal1Inclusive, temporal2Exclusive);
      }
    }
  }

  private static class Quarters implements TemporalUnit {
    @Override
    public Duration getDuration() {
      return ChronoUnit.MONTHS.getDuration().multipliedBy(3);
    }

    @Override
    public boolean isDurationEstimated() {
      return true;
    }

    @Override
    public boolean isDateBased() {
      return true;
    }

    @Override
    public boolean isTimeBased() {
      return false;
    }

    @Override
    public <R extends Temporal> R addTo(R temporal, long amount) {
      return ChronoUnit.MONTHS.addTo(temporal, amount * 3);
    }

    @Override
    public long between(Temporal temporal1Inclusive, Temporal temporal2Exclusive) {
      return ChronoUnit.MONTHS.between(temporal1Inclusive, temporal2Exclusive) / 3;
    }
  }
}
