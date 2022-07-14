package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import java.io.IOException;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import java.time.format.DateTimeParseException;

import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
@Builtin(pkg = "date", name = "Date")
public final class EnsoDate implements TruffleObject {
  private final LocalDate date;

  public EnsoDate(LocalDate date) {
    this.date = date;
  }

  @Builtin.Method(description = "Return current Date")
  public static EnsoDate now() {
    return new EnsoDate(LocalDate.now());
  }

  /**
   * TODO: This should work but seems that annotation doesn't pick up String conversion correctly
   *
  @Builtin.Method(name = "date_parse")
  @Builtin.Specialize
  public static EnsoDate parse(Text text, String pattern) {
    var formatter = DateTimeFormatter.ofPattern(pattern);
    return new EnsoDate(LocalDate.parse(null, formatter));
  }
   */

  @Builtin.Method(name = "date_new", description = "Constructs a new Date from a year, month, and day")
  @Builtin.WrapException(from = DateTimeException.class, to = PolyglotError.class, propagate = true)
  public static EnsoDate create(long year, long month, long day) {
    return new EnsoDate(LocalDate.of(Math.toIntExact(year), Math.toIntExact(month), Math.toIntExact(day)));
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
  boolean hasMembers() {
    return true;
  }

  @ExportMessage
  Object getMembers(boolean internal) {
    return new Array("year", "month", "day");
  }

  @ExportMessage
  boolean isMemberReadable(String member) {
    return switch (member) {
      case "year" -> true;
      case "month" -> true;
      case "day" -> true;
      default -> false;
    };
  }

  @ExportMessage
  int readMember(String member, @Cached("member") String cachedMember) throws UnknownIdentifierException {
    return switch (member) {
      case "year" -> date.getYear();
      case "month" -> date.getMonthValue();
      case "day" -> date.getDayOfMonth();
      default -> throw UnknownIdentifierException.create(cachedMember);
    };
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
