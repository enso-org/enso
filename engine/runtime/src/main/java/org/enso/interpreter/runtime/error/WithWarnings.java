package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

@ExportLibrary(MethodDispatchLibrary.class)
public class WithWarnings implements TruffleObject {
  private final ArrayRope<Warning> warnings;
  private final Object value;

  public WithWarnings(Object value, Warning... warnings) {
    this.warnings = new ArrayRope<>(warnings);
    this.value = value;
  }

  private WithWarnings(Object value, ArrayRope<Warning> warnings) {
    this.warnings = warnings;
    this.value = value;
  }

  public Object getValue() {
    return value;
  }

  public WithWarnings append(Warning... newWarnings) {
    return new WithWarnings(value, warnings.append(newWarnings));
  }

  public WithWarnings prepend(Warning... newWarnings) {
    return new WithWarnings(value, warnings.prepend(newWarnings));
  }

  public Warning[] getWarningsArray() {
    return warnings.toArray(Warning[]::new);
  }

  public ArrayRope<Warning> getWarnings() {
    return warnings;
  }

  public ArrayRope<Warning> getReassignedWarnings(Context ctx, Node location) {
    Warning[] warnings = getWarningsArray();
    for (int i = 0; i < warnings.length; i++) {
      warnings[i] = warnings[i].reassign(ctx, location);
    }
    return new ArrayRope<>(warnings);
  }

  public static WithWarnings appendTo(Object target, ArrayRope<Warning> warnings) {
    if (target instanceof WithWarnings) {
      return new WithWarnings(((WithWarnings) target).warnings.append(warnings));
    } else {
      return new WithWarnings(target, warnings);
    }
  }

  public static WithWarnings prependTo(Object target, ArrayRope<Warning> warnings) {
    if (target instanceof WithWarnings) {
      return new WithWarnings(((WithWarnings) target).warnings.prepend(warnings));
    } else {
      return new WithWarnings(target, warnings);
    }
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }
}
