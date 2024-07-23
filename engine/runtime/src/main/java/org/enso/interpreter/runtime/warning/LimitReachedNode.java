package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Warning",
    name = "limit_reached",
    description =
        "Returns `true` if the maximal number of warnings has been reached, `false` otherwise.",
    autoRegister = false)
public abstract class LimitReachedNode extends Node {
  public static LimitReachedNode build() {
    return LimitReachedNodeGen.create();
  }

  public abstract boolean execute(@AcceptsWarning Object value);

  @Specialization
  boolean doWithWarns(WithWarnings withWarns) {
    return withWarns.isLimitReached();
  }

  @Fallback
  boolean doGeneric(Object value, @CachedLibrary(limit = "3") WarningsLibrary warnsLib) {
    return warnsLib.hasWarnings(value) && warnsLib.isLimitReached(value);
  }
}
