package org.enso.interpreter.node.expression.foreign;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;

@GenerateUncached
public abstract class CoerceNothing extends Node {
  public static CoerceNothing build() {
    return CoerceNothingNodeGen.create();
  }

  /**
   * Converts a null polyglot representation into an equivalent Nothing representation in Enso
   * context.
   *
   * @param value the polyglot value to perform coercion on
   * @return {@code value} coerced to an Enso primitive where applicable
   */
  public abstract Object execute(Object value);

  @Specialization(guards = "interop.isNull(value)")
  public Object doNothing(
      Object value,
      @CachedLibrary(limit = "1") InteropLibrary interop,
      @CachedLibrary(limit = "3") WarningsLibrary warningsLibrary,
      @Cached("createCountingProfile()") ConditionProfile nullWarningProfile) {
    var nothing = EnsoContext.get(this).getBuiltins().nothing();
    if (nullWarningProfile.profile(warningsLibrary.hasWarnings(value))) {
      try {
        Warning[] attachedWarnings = warningsLibrary.getWarnings(value, null);
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
