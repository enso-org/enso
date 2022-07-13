package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.time.DateTimeException;
import java.time.LocalDate;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Date_Internal",
    name = "date_new",
    description = "Constructs a new Date from a year, month, and day")
public abstract class NewNode extends Node {
  static NewNode build() {
    return NewNodeGen.create();
  }

  abstract Object execute(Object self, long year, long month, long day);

  @Specialization
  Object doNew(Object self, long year, long month, long day) {
    try {
      return new EnsoDate(LocalDate.of(Math.toIntExact(year), Math.toIntExact(month), Math.toIntExact(day)));
    } catch (DateTimeException ex) {
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(ex.getMessage(), this);
    }
  }
}
