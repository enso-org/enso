package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

import java.time.DateTimeException;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.zone.ZoneRulesException;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
@Builtin(pkg = "date", name = "TimeZone", stdlibName = "Standard.Base.Data.Time.Time_Zone")
public final class EnsoTimeZone implements TruffleObject {
  private final ZoneId zone;

  public EnsoTimeZone(ZoneId zone) {
    this.zone = zone;
  }

  @Builtin.Method(description = "Get the unique identifier for your system's current timezone.")
  public Text zoneId() {
    return Text.create(this.zone.getId());
  }

  @Builtin.Method(name = "parse_builtin", description = "Parse the ID producing a Time_Zone.")
  @Builtin.Specialize
  @Builtin.WrapException(
      from = ZoneRulesException.class,
      to = PolyglotError.class,
      propagate = true)
  public static EnsoTimeZone parse(String text) {
    return new EnsoTimeZone(ZoneId.of(text));
  }

  @Builtin.Method(
      name = "new_builtin",
      description =
          "Obtains an instance of `Time_Zone` using an offset in hours, minutes and seconds from the UTC zone.")
  @Builtin.WrapException(from = DateTimeException.class, to = PolyglotError.class, propagate = true)
  public static EnsoTimeZone create(long hours, long minutes, long seconds) {
    return new EnsoTimeZone(
        ZoneOffset.ofHoursMinutesSeconds(
            Math.toIntExact(hours), Math.toIntExact(minutes), Math.toIntExact(seconds)));
  }

  @Builtin.Method(name = "system", description = "The system default timezone.")
  public static EnsoTimeZone system() {
    return new EnsoTimeZone(ZoneId.systemDefault());
  }

  @ExportMessage
  boolean isTimeZone() {
    return true;
  }

  @ExportMessage
  ZoneId asTimeZone() {
    return zone;
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
      return symbol.resolveFor(context.getBuiltins().timeZone(), context.getBuiltins().any());
    }

    @Specialization(
        guards = {"cachedSymbol == symbol", "function != null"},
        limit = "3")
    static Function resolveCached(
        EnsoTimeZone self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @CachedLibrary("self") InteropLibrary mySelf,
        @Cached("doResolve(mySelf, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
        EnsoTimeZone self, UnresolvedSymbol symbol, @CachedLibrary("self") InteropLibrary mySelf)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(mySelf, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }
}
