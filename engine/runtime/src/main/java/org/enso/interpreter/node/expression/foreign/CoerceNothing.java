package org.enso.interpreter.node.expression.foreign;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.runtime.EnsoContext;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;

public abstract class CoerceNothing extends Node {
  public static CoerceNothing build() {
    return CoerceNothingNodeGen.create();
  }

  /**
   * Converts an null polyglot representation into an equivalent Nothing representation in Enso
   * context.
   *
   * @param value the polyglot value to perform coercion on
   * @return {@code value} coerced to an Enso primitive where applicable
   */
  public abstract Object execute(Object value);

  @Specialization(guards = "interop.isNull(value)")
  public Object doNothing(Object value, @CachedLibrary(limit = "1") InteropLibrary interop, @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    var nothing = EnsoContext.get(this).getBuiltins().nothing();

    if (warnings.hasWarnings(value)) {
      try {
        Warning[] attachedWarnings = warnings.getWarnings(value, null);
        return new WithWarnings(nothing, attachedWarnings);
      } catch (UnsupportedMessageException e) {
        return nothing;
      }
    }

    return nothing;
  }

  @Fallback
  public Object doOther(Object value) {
    return value;
  }
}
