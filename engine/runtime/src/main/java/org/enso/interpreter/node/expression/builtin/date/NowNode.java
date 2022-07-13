package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.time.LocalDate;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Date_Internal",
    name = "date_create",
    description = "Returns current Date")
public abstract class NowNode extends Node {
  static NowNode build() {
    return NowNodeGen.create();
  }

  abstract Object execute(Object self, long type);

  @Specialization
  Object doNow(Object self) {
    return new EnsoDate(LocalDate.now());
  }
}
