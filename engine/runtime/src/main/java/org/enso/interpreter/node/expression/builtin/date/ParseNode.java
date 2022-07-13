package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.time.LocalDate;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Date_Internal",
    name = "date_parse",
    description = "Constructs a new Date from a year, month, and day")
public abstract class ParseNode extends Node {
  static ParseNode build() {
    return ParseNodeGen.create();
  }

  abstract Object execute(Object self, Text text, Object pattern);

  @Specialization
  Object doParse(Object self, Text text, Object pattern) {
    return self;
  }
}
