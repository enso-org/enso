package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;

@BuiltinMethod(
    type = "Date_Internal",
    name = "date_value",
    description = "Returns some value for a Date")
public abstract class YearMonthDayNode extends Node {
  public static YearMonthDayNode build() {
    return YearMonthDayNodeGen.create();
  }

  abstract int execute(@MonadicState Object state, Object self, long type, Object date);

  @Specialization(guards = "type == 1")
  int executeYear(
      Object state,
      Object self,
      long type,
      Object date,
      @CachedLibrary(limit="3") InteropLibrary iop
  ) {
    try {
      return iop.asDate(date).getYear();
    } catch (UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @Specialization(guards = "type == 2")
  int executeMonth(
      Object state,
      Object self,
      long type,
      Object date,
      @CachedLibrary(limit="3") InteropLibrary iop
  ) {
    try {
      return iop.asDate(date).getMonthValue();
    } catch (UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @Specialization(guards = "type == 3")
  int executeDay(
      Object state,
      Object self,
      long type,
      Object date,
      @CachedLibrary(limit="3") InteropLibrary iop
  ) {
    try {
      return iop.asDate(date).getDayOfMonth();
    } catch (UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }
}
