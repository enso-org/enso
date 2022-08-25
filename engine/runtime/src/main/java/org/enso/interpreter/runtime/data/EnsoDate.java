package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
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
import java.time.format.DateTimeParseException;

import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
@Builtin(pkg = "date", name = "Date", stdlibName = "Standard.Base.Data.Time.Date")
public final class EnsoDate implements TruffleObject {
  private final LocalDate date;

  public EnsoDate(LocalDate date) {
    this.date = date;
  }

  @Builtin.Method(description = "Return current Date")
  public static EnsoDate now() {
    return new EnsoDate(LocalDate.now());
  }

  @Builtin.Method(name = "internal_parse", description = "Constructs a new Date from text with optional pattern")
  @Builtin.Specialize
  @Builtin.WrapException(from = DateTimeParseException.class, to = PolyglotError.class, propagate = true)
  public static EnsoDate parse(Text text, Object noneOrPattern) {
    var str = text.getContents().toString();
    if (noneOrPattern instanceof Text pattern) {
      var formatter = DateTimeFormatter.ofPattern(pattern.getContents().toString());
      return new EnsoDate(LocalDate.parse(str, formatter));
    } else {
      return new EnsoDate(LocalDate.parse(str));
    }
  }

  @Builtin.Method(name = "internal_new", description = "Constructs a new Date from a year, month, and day")
  @Builtin.WrapException(from = DateTimeException.class, to = PolyglotError.class, propagate = true)
  public static EnsoDate create(long year, long month, long day) {
    return new EnsoDate(LocalDate.of(Math.toIntExact(year), Math.toIntExact(month), Math.toIntExact(day)));
  }

  @Builtin.Method(name = "year", description = "Gets a value of year")
  public long year() {
    return date.getYear();
  }

  @Builtin.Method(name = "month", description = "Gets a value month")
  public long month() {
    return date.getMonthValue();
  }

  @Builtin.Method(name = "day", description = "Gets a value day")
  public long day() {
    return date.getDayOfMonth();
  }

  @Builtin.Method(name = "to_time_builtin", description = "Combine this day with time to create a point in time.")
  public EnsoDateTime toTime(EnsoTimeOfDay timeOfDay, EnsoZone zone) {
    return new EnsoDateTime(date.atTime(timeOfDay.asTime()).atZone(zone.asTimeZone()));
  }

  @ExportMessage
  boolean isDate() {
    return true;
  }

  @ExportMessage
  LocalDate asDate() {
    return date;
  }

  @ExportMessage
  boolean isTime() {
    return false;
  }

  @ExportMessage
  LocalTime asTime() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {
    @CompilerDirectives.TruffleBoundary
    static Function doResolve(InteropLibrary my, UnresolvedSymbol symbol) {
      Context context = Context.get(my);
      return symbol.resolveFor(context.getBuiltins().date(), context.getBuiltins().any());
    }

    @Specialization(
        guards = {"cachedSymbol == symbol", "function != null"},
        limit = "3")
    static Function resolveCached(
        EnsoDate self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @CachedLibrary("self") InteropLibrary mySelf,
        @Cached("doResolve(mySelf, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
        EnsoDate self, UnresolvedSymbol symbol, @CachedLibrary("self") InteropLibrary mySelf)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(mySelf, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  public final Object toDisplayString(boolean allowSideEffects) {
    return DateTimeFormatter.ISO_LOCAL_DATE.format(date);
  }
}
