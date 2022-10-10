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
  public static EnsoDuration between(
      Object startInclusive, Object endExclusive, boolean timeZoneAware, InteropLibrary interop) {
    Temporal startTime = convertToDateTime(startInclusive, timeZoneAware, interop);
    Temporal endTime = convertToDateTime(endExclusive, timeZoneAware, interop);
    return new EnsoDuration(Duration.between(startTime, endTime));
  }

  private static Temporal convertToDateTime(
      Object dateObject, boolean timeZoneAware, InteropLibrary interop) {
    assert interop.isDate(dateObject);
    try {
      LocalDate date = interop.asDate(dateObject);
      LocalTime time = interop.isTime(dateObject) ? interop.asTime(dateObject) : LocalTime.MIN;
      if (timeZoneAware) {
        ZoneId zone =
            interop.isTimeZone(dateObject)
                ? interop.asTimeZone(dateObject)
                : ZoneId.systemDefault();
        return ZonedDateTime.of(date, time, zone);
      } else {
        return LocalDateTime.of(date, time);
      }
    } catch (UnsupportedMessageException e) {
      throw new PanicException(e.getMessage(), interop);
    }
  }

  @Builtin.Method(description = "Gets the hours")
  @CompilerDirectives.TruffleBoundary
  public long hours() {
    return duration.toHours();
  }

  @Builtin.Method(description = "Gets the minutes")
  @CompilerDirectives.TruffleBoundary
  public long minutes() {
    return duration.toMinutes();
  }

  @Builtin.Method(description = "Gets the seconds")
  @CompilerDirectives.TruffleBoundary
  public long seconds() {
    return duration.toSeconds();
  }

  @Builtin.Method(description = "Gets the milliseconds")
  @CompilerDirectives.TruffleBoundary
  public long milliseconds() {
    return duration.toMillis();
  }

  @Builtin.Method(description = "Gets the nanoseconds")
  @CompilerDirectives.TruffleBoundary
  public long nanoseconds() {
    return duration.toNanos();
  }

  @Builtin.Method(name = "plus_builtin", description = "Adds another Duration")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class, to = PanicException.class)
  public EnsoDuration plus(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    return new EnsoDuration(duration.plus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(name = "minus_builtin", description = "Subtracts another Duration")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class, to = PanicException.class)
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
