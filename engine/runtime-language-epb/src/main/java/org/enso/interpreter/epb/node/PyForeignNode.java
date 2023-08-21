package org.enso.interpreter.epb.node;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;

import org.enso.interpreter.epb.EpbContext;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.source.Source;

@NodeField(name = "foreignFunction", type = Object.class)
public abstract class PyForeignNode extends ForeignFunctionCallNode {

  private @Child CoercePrimitiveNode coercePrimitiveNode = CoercePrimitiveNode.build();

  abstract Object getForeignFunction();

  @CompilerDirectives.TruffleBoundary
  private Object wrapPythonDate(LocalDate date) {

    var ctx = EpbContext.get(this);
    var src = Source.newBuilder("python", """
    from datetime import date
    date
    """, "convert_date.py").build();

    var conv = ctx.getEnv().parsePublic(src).call();

    try {
      return InteropLibrary.getUncached().execute(conv, date.getYear(), date.getMonthValue(), date.getDayOfMonth());
    } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object wrapPythonTime(LocalTime time) {
    var ctx = EpbContext.get(this);
    var src = Source.newBuilder("python", """
    from datetime import time
    time
    """, "convert_time.py").build();

    var conv = ctx.getEnv().parsePublic(src).call();

    try {
      return InteropLibrary.getUncached().execute(conv, time.getHour(), time.getMinute(), time.getSecond(), time.getNano() / 1000);
    } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object combinePythonDateTimeZone(Object date, Object time, Object zone) {
    var ctx = EpbContext.get(this);
    var src = Source.newBuilder("python", """
    from datetime import datetime
    datetime.combine
    """, "convert_combine.py").build();

    var conv = ctx.getEnv().parsePublic(src).call();

    try {
      if (zone != null) {
        return InteropLibrary.getUncached().execute(conv, date, time, zone);
      } else {
        return InteropLibrary.getUncached().execute(conv, date, time);
      }
    } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object wrapPythonZone(ZoneId zone, LocalTime time, LocalDate date) {
    var ctx = EpbContext.get(this);
    var src = Source.newBuilder("python", """
    from datetime import timezone, timedelta
    def conv(sec):
        d = timedelta(seconds=sec)
        return timezone(d)
    conv
    """, "convert_time_zone.py").build();

    var conv = ctx.getEnv().parsePublic(src).call();

    Instant when;
    if (time != null) {
      if (date != null) {
        when = date.atTime(time).atZone(zone).toInstant();
      } else {
        when = Instant.now();
      }
    } else {
      if (date != null) {
        when = date.atStartOfDay(zone).toInstant();
      } else {
        when = Instant.now();
      }
    }
    try {
      return InteropLibrary.getUncached().execute(conv, zone.getRules().getOffset(when).getTotalSeconds());
    } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @Specialization
  public Object doExecute(
      Object[] arguments,
      @CachedLibrary("foreignFunction") InteropLibrary interopLibrary,
      @CachedLibrary(limit="3") InteropLibrary iop
  ) {
    try {
      for (int i = 0; i < arguments.length; i++) {
        var javaTime = iop.isTime(arguments[i]) ? iop.asTime(arguments[i]) : null;
        var time = javaTime != null ? wrapPythonTime(javaTime) : null;
        var javaDate = iop.isDate(arguments[i]) ? iop.asDate(arguments[i]) : null;
        var date = javaDate != null ? wrapPythonDate(javaDate) : null;
        var zone = iop.isTimeZone(arguments[i]) ? wrapPythonZone(iop.asTimeZone(arguments[i]), javaTime, javaDate) : null;
        if (date != null && time != null) {
          arguments[i] = combinePythonDateTimeZone(date, time, zone);
        } else if (date != null) {
          arguments[i] = date;
        } else if (time != null) {
          arguments[i] = time;
        } else if (zone != null) {
          arguments[i] = zone;
        }
      }
      return coercePrimitiveNode.execute(interopLibrary.execute(getForeignFunction(), arguments));
    } catch (UnsupportedMessageException | UnsupportedTypeException | ArityException e) {
      throw new IllegalStateException("Python parser returned a malformed object", e);
    }
  }
}
