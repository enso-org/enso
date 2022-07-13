package org.enso.interpreter.node.expression.builtin.date;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Date_Internal",
    name = "date_to_enso",
    description = "Converts any format of a Date to Enso Date")
public abstract class ToEnsoDateNode extends Node {
  public static ToEnsoDateNode build() {
    return ToEnsoDateNodeGen.create();
  }

  abstract EnsoDate execute(Object self, Object date);

  @Specialization
  EnsoDate executeConversion(
      Object self,
      Object date,
      @CachedLibrary(limit="3") InteropLibrary iop
  ) {
    try {
      return new EnsoDate(iop.asDate(date));
    } catch (UnsupportedMessageException ex) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(ex);
    }
  }
}
