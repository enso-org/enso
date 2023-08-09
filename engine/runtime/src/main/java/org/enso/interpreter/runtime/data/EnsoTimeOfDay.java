package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(
    pkg = "date",
    name = "TimeOfDay",
    stdlibName = "Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day")
public final class EnsoTimeOfDay implements TruffleObject {
  private final LocalTime localTime;

  public EnsoTimeOfDay(LocalTime localTime) {
    this.localTime = localTime;
  }

  @Builtin.Method(
      name = "new_builtin",
      description = "Constructs a new Time_OF_Day from an hour",
      autoRegister = false)
  @Builtin.WrapException(from = DateTimeException.class)
  @CompilerDirectives.TruffleBoundary
  public static EnsoTimeOfDay create(long hour, long minute, long second, long nanosecond) {
    return new EnsoTimeOfDay(
        LocalTime.of(
            Math.toIntExact(hour),
            Math.toIntExact(minute),
            Math.toIntExact(second),
            Math.toIntExact(nanosecond)));
  }

  @Builtin.Method(description = "Gets a value of hour", autoRegister = false)
  @CompilerDirectives.TruffleBoundary
  public static EnsoTimeOfDay now() {
    return new EnsoTimeOfDay(LocalTime.now());
  }

  @Builtin.Method(description = "Gets a value of hour")
  public long hour() {
    return localTime.getHour();
  }

  @Builtin.Method(description = "Gets a value minute")
  public long minute() {
    return localTime.getMinute();
  }

  @Builtin.Method(description = "Gets a value second")
  public long second() {
    return localTime.getSecond();
  }

  @Builtin.Method(description = "Gets the millisecond")
  @CompilerDirectives.TruffleBoundary
  public long millisecond() {
    return localTime.getNano() / 1000_000;
  }

  @Builtin.Method(description = "Gets the microsecond")
  @CompilerDirectives.TruffleBoundary
  public long microsecond() {
    return (localTime.getNano() / 1000) % 1000;
  }

  @Builtin.Method(name = "nanosecond_builtin", description = "Gets the nanosecond")
  @CompilerDirectives.TruffleBoundary
  public long nanosecond(boolean includeMilliseconds) {
    long nanos = localTime.getNano();
    if (includeMilliseconds) {
      return nanos;
    } else {
      return nanos % 1000;
    }
  }

  @Builtin.Method(name = "plus_builtin", description = "Adds a duration to this Time_Of_Day")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  @TruffleBoundary
  public EnsoTimeOfDay plus(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    assert interop.isDuration(durationObject);
    return new EnsoTimeOfDay(localTime.plus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(
      name = "minus_builtin",
      description = "Subtracts a duration from this Time_Of_Day")
  @Builtin.Specialize
  @Builtin.WrapException(from = UnsupportedMessageException.class)
  @TruffleBoundary
  public EnsoTimeOfDay minus(Object durationObject, InteropLibrary interop)
      throws UnsupportedMessageException {
    assert interop.isDuration(durationObject);
    return new EnsoTimeOfDay(localTime.minus(interop.asDuration(durationObject)));
  }

  @Builtin.Method(description = "Gets a value second")
  @CompilerDirectives.TruffleBoundary
  public long toSeconds() {
    return localTime.toSecondOfDay();
  }

  @Builtin.Method(description = "Return this datetime to the datetime in the provided time zone.")
  @CompilerDirectives.TruffleBoundary
  public Text toText() {
    return Text.create(DateTimeFormatter.ISO_LOCAL_TIME.format(localTime));
  }

  @ExportMessage
  boolean isTime() {
    return true;
  }

  @ExportMessage
  LocalTime asTime() {
    return localTime;
  }

  @ExportMessage
  boolean isDate() {
    return false;
  }

  @ExportMessage
  LocalDate asDate() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().timeOfDay();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().timeOfDay();
  }

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  public Object toDisplayString(boolean allowSideEffects) {
    return DateTimeFormatter.ISO_LOCAL_TIME.format(localTime);
  }
}
