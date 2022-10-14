package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.Temporal;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "date", name = "Duration", stdlibName = "Standard.Base.Data.Time.Duration")
public final class EnsoDuration implements TruffleObject {
  private final Duration duration;

  public EnsoDuration(Duration duration) {
    this.duration = duration;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().duration();
  }

  @Builtin.Method(
      name = "new_builtin",
      description =
          "Constructs a new Duration from hours, minutes, seconds, milliseconds and nanoseconds")
  @TruffleBoundary
  public static EnsoDuration create(
      long hours, long minutes, long seconds, long milliseconds, long nanoseconds) {
    var duration =
        Duration.ofHours(hours)
            .plusMinutes(minutes)
            .plusSeconds(seconds)
            .plusMillis(milliseconds)
            .plusNanos(nanoseconds);
    return new EnsoDuration(duration);
  }

  @Builtin.Method(
      name = "between_builtin",
      description =
          "Construct a new Duration that is between the given start date inclusive, and end date exclusive")
  @Builtin.Specialize
  @TruffleBoundary
  public static EnsoDuration between(
      Object startInclusive, Object endExclusive, boolean timeZoneAware, InteropLibrary interop) {
    if (!isDateTime(startInclusive, interop)
        || (timeZoneAware && !hasTimeZone(startInclusive, interop))) {
      throw createNotDateTimePanic("start_inclusive", startInclusive, interop);
    }
    if (!isDateTime(endExclusive, interop)
        || (timeZoneAware && !hasTimeZone(endExclusive, interop))) {
      throw createNotDateTimePanic("end_exclusive", endExclusive, interop);
    }
    Temporal startTime = convertToDateTime(startInclusive, timeZoneAware, interop);
    Temporal endTime = convertToDateTime(endExclusive, timeZoneAware, interop);
    return new EnsoDuration(Duration.between(startTime, endTime));
  }

  private static boolean isDateTime(Object dateTime, InteropLibrary interop) {
    return interop.isDate(dateTime) && interop.isTime(dateTime);
  }

  private static boolean hasTimeZone(Object dateTime, InteropLibrary interop) {
    return interop.isTimeZone(dateTime);
  }

  private static PanicException createNotDateTimePanic(
      String varName, Object object, InteropLibrary interop) {
    return new PanicException(
        Context.get(interop).getBuiltins().error().makeTypeError("Date_Time", object, varName),
        interop);
  }

  private static Temporal convertToDateTime(
      Object dateTimeObject, boolean timeZoneAware, InteropLibrary interop) {
    assert interop.isDate(dateTimeObject);
    assert interop.isTime(dateTimeObject);
    try {
      LocalDate date = interop.asDate(dateTimeObject);
      LocalTime time = interop.asTime(dateTimeObject);
      if (timeZoneAware) {
        assert interop.isTime(dateTimeObject);
        ZoneId zone = interop.asTimeZone(dateTimeObject);
        return ZonedDateTime.of(date, time, zone);
      } else {
        return LocalDateTime.of(date, time);
      }
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Builtin.Method(description = "Gets the hours part")
  public long hours() {
    return duration.toHoursPart();
  }

  @Builtin.Method(description = "Gets the minutes part")
  public long minutes() {
    return duration.toMinutesPart();
  }

  @Builtin.Method(description = "Gets the seconds part")
  public long seconds() {
    return duration.toSecondsPart();
  }

  @Builtin.Method(description = "Gets the milliseconds part")
  public long milliseconds() {
    return duration.toMillisPart();
  }

  @Builtin.Method(description = "Gets the nanoseconds part")
  public long nanoseconds() {
    return duration.toNanosPart();
  }

  @Builtin.Method(
      name = "total_milliseconds_builtin",
      description = "Gets the total amount of milliseconds")
  public long totalMilliseconds() {
    return duration.toMillis();
  }

  @Builtin.Method(name = "plus_builtin", description = "Adds another Duration")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class, to = PanicException.class)
  @TruffleBoundary
  public EnsoDuration plus(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return new EnsoDuration(duration.plus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(name = "minus_builtin", description = "Subtracts another Duration")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class, to = PanicException.class)
  @TruffleBoundary
  public EnsoDuration minus(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return new EnsoDuration(duration.minus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(name = "compare_to_builtin", description = "Compares to other duration")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class, to = PanicException.class)
  public long compareTo(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return duration.compareTo(interop.asDuration(durationObject));
  }

  @Builtin.Method(name = "equals_builtin")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class, to = PanicException.class)
  public boolean equalsDuration(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return duration.equals(interop.asDuration(durationObject));
  }

  @ExportMessage
  public boolean isDuration() {
    return true;
  }

  @ExportMessage
  public Duration asDuration() {
    return duration;
  }

  @ExportMessage
  @TruffleBoundary
  public String toDisplayString(boolean allowSideEffects) {
    return duration.toString();
  }
}
