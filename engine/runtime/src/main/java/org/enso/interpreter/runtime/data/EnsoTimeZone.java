package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.error.PolyglotError;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.time.DateTimeException;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.zone.ZoneRulesException;

@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
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
  @Builtin.WrapException(from = ZoneRulesException.class, to = PolyglotError.class)
  public static EnsoTimeZone parse(String text) {
    return new EnsoTimeZone(ZoneId.of(text));
  }

  @Builtin.Method(
      name = "new_builtin",
      description =
          "Obtains an instance of `Time_Zone` using an offset in hours, minutes and seconds from the UTC zone.")
  @Builtin.WrapException(from = DateTimeException.class, to = PolyglotError.class)
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
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().timeZone();
  }
}
