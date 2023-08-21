package org.enso.interpreter.epb.node;

import java.time.LocalDate;
import java.time.LocalTime;

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
  private Object combinePythonDateTime(Object date, Object time) {
    var ctx = EpbContext.get(this);
    var src = Source.newBuilder("python", """
    from datetime import datetime
    datetime.combine
    """, "convert_combine.py").build();

    var conv = ctx.getEnv().parsePublic(src).call();

    try {
      return InteropLibrary.getUncached().execute(conv, date, time);
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
        var time = iop.isTime(arguments[i]) ? wrapPythonTime(iop.asTime(arguments[i])) : null;
        var date = iop.isDate(arguments[i]) ? wrapPythonDate(iop.asDate(arguments[i])) : null;
        if (date != null && time != null) {
          arguments[i] = combinePythonDateTime(date, time);
        } else if (date != null) {
          arguments[i] = date;
        } else if (time != null) {
          arguments[i] = time;
        }
      }
      return coercePrimitiveNode.execute(interopLibrary.execute(getForeignFunction(), arguments));
    } catch (UnsupportedMessageException | UnsupportedTypeException | ArityException e) {
      throw new IllegalStateException("Python parser returned a malformed object", e);
    }
  }
}
