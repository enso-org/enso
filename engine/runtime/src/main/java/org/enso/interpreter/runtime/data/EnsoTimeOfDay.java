package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "date", name = "TimeOfDay", stdlibName = "Standard.Base.Data.Time.Time_Of_Day")
public class EnsoTimeOfDay implements TruffleObject {
  private LocalTime localTime;

  public EnsoTimeOfDay(LocalTime localTime) {
    this.localTime = localTime;
  }

  @Builtin.Method(
      name = "parse_builtin",
      description = "Constructs a new DateTime from text with optional pattern")
  @Builtin.Specialize
  @Builtin.WrapException(from = DateTimeParseException.class, to = PolyglotError.class)
  public static EnsoTimeOfDay parse(String text) {
    return new EnsoTimeOfDay(LocalTime.parse(text));
  }

  @Builtin.Method(name = "new_builtin", description = "Constructs a new Time_OF_Day from an hour")
  @Builtin.WrapException(from = DateTimeException.class, to = PolyglotError.class)
  public static EnsoTimeOfDay create(long hour, long minute, long second, long nanosecond) {
    return new EnsoTimeOfDay(
        LocalTime.of(
            Math.toIntExact(hour),
            Math.toIntExact(minute),
            Math.toIntExact(second),
            Math.toIntExact(nanosecond)));
  }

  @Builtin.Method(description = "Gets a value of hour")
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

  @Builtin.Method(description = "Gets a value nanosecond")
  public long nanosecond() {
    return localTime.getNano();
  }

  @Builtin.Method(description = "Gets a value second")
  public long toSeconds() {
    return localTime.toSecondOfDay();
  }

  @Builtin.Method(
      name = "to_time_builtin",
      description = "Combine this time of day with a date to create a point in time.")
  public EnsoDateTime toTime(EnsoDate date, EnsoTimeZone zone) {
    return new EnsoDateTime(localTime.atDate(date.asDate()).atZone(zone.asTimeZone()));
  }

  @Builtin.Method(description = "Return this datetime to the datetime in the provided time zone.")
  public Text toText() {
    return Text.create(DateTimeFormatter.ISO_LOCAL_TIME.format(localTime));
  }

  @Builtin.Method(description = "Return this datetime to the datetime in the provided time zone.")
  @Builtin.Specialize
  public Text format(String pattern) {
    return Text.create(DateTimeFormatter.ofPattern(pattern).format(localTime));
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
  final boolean isDate() {
    return false;
  }

  @ExportMessage
  LocalDate asDate() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().timeOfDay();
  }
}
