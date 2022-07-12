package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;

@BuiltinMethod(
    type = "Date",
    name = "year",
    description = "Returns year of a Date")
public abstract class YearNode extends Node {
  public static YearNode build() {
    return YearNodeGen.create();
  }

  abstract int execute(@MonadicState Object state, Object self);

  @Specialization
  int executePolyglotDate(
      Object state,
      Object self,
      @CachedLibrary(limit="3") InteropLibrary iop
  ) {
    try {
      return iop.asDate(self).getYear();
    } catch (UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }
}
