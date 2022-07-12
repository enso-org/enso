package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.time.LocalDate;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Date", name = "==", description = "Computes the equality of two dates")
public abstract class EqualsNode extends Node {
  abstract boolean execute(Object self, Object that);

  static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  @Specialization
  boolean doLocalDate(LocalDate self, LocalDate that) {
    if (self == null) {
      return that == null;
    }
    return self.equals(that);
  }

  @Fallback
  boolean doOther(Object self, Object that) {
    return false;
  }
}
