package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.time.LocalDate;
import org.enso.interpreter.dsl.BuiltinMethod;

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
    return new EnsoDate(LocalDate.of(Math.toIntExact(year), Math.toIntExact(month), Math.toIntExact(day)));
  }
}
