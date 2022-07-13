package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.EnsoDate;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Date",
    name = "date_parse",
    description = "Constructs a new Date from a year, month, and day")
public abstract class ParseNode extends Node {
  static ParseNode build() {
    return ParseNodeGen.create();
  }

  abstract Object execute(Object self, Text text, Object pattern);

  @Specialization
  Object doParse(Object self, Text text, Long noPattern) {
    try {
      return new EnsoDate(LocalDate.parse(text.toString()));
    } catch (DateTimeParseException ex) {
      throw new PanicException(ex, this);
    }
  }

  @Specialization
  Object doParse(Object self, Text text, Text pattern) {
    try {
      var formatter = DateTimeFormatter.ofPattern(pattern.toString());
      return new EnsoDate(LocalDate.parse(text.toString(), formatter));
    } catch (DateTimeParseException ex) {
      throw new PanicException(ex.getMessage(), this);
    }
  }
}
