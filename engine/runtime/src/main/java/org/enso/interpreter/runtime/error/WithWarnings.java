package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
@ExportLibrary(ReflectionLibrary.class)
public final class WithWarnings implements TruffleObject {
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

  public WithWarnings append(ArrayRope<Warning> newWarnings) {
    return new WithWarnings(value, warnings.append(newWarnings));
  }

  public WithWarnings prepend(Warning... newWarnings) {
    return new WithWarnings(value, warnings.prepend(newWarnings));
  }

  public WithWarnings prepend(ArrayRope<Warning> newWarnings) {
    return new WithWarnings(value, warnings.prepend(newWarnings));
  }

  public Warning[] getWarningsArray(WarningsLibrary warningsLibrary) {
    Warning[] warningsArr = warnings.toArray(Warning[]::new);
    Warning[] allWarnings;

    if (warningsLibrary != null && warningsLibrary.hasWarnings(value)) {
      try {
        Warning[] valuesWarnings = warningsLibrary.getWarnings(value, null);
        allWarnings = new Warning[valuesWarnings.length + warningsArr.length];
        System.arraycopy(warningsArr, 0, allWarnings, 0, warningsArr.length);
        System.arraycopy(valuesWarnings, 0, allWarnings, warningsArr.length, valuesWarnings.length);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(e);
      }
    } else {
      allWarnings = warningsArr;
    }
    return allWarnings;
  }

  /** @return the number of warnings. */
  public int getWarningsCount() {
    return warnings.size();
  }

  public ArrayRope<Warning> getReassignedWarnings(Node location) {
    return getReassignedWarnings(location, null);
  }

  public ArrayRope<Warning> getReassignedWarnings(Node location, WarningsLibrary warningsLibrary) {
    Warning[] warnings = getWarningsArray(warningsLibrary);
    for (int i = 0; i < warnings.length; i++) {
      warnings[i] = warnings[i].reassign(location);
    }
    return new ArrayRope<>(warnings);
  }

  public static WithWarnings appendTo(Object target, ArrayRope<Warning> warnings) {
    if (target instanceof WithWarnings) {
      return ((WithWarnings) target).append(warnings);
    } else {
      return new WithWarnings(target, warnings);
    }
  }

  public static WithWarnings prependTo(Object target, ArrayRope<Warning> warnings) {
    if (target instanceof WithWarnings) {
      return ((WithWarnings) target).prepend(warnings);
    } else {
      return new WithWarnings(target, warnings);
    }
  }

  @ExportMessage
  Object send(Message message, Object[] args, @CachedLibrary(limit = "3") ReflectionLibrary lib)
      throws Exception {
    return lib.send(value, message, args);
  }

  @ExportMessage
  boolean hasWarnings() {
    return warnings.size() > 0;
  }

  @ExportMessage
  Warning[] getWarnings(
      Node location, @CachedLibrary(limit = "3") WarningsLibrary warningsLibrary) {
    if (location != null) {
      return getReassignedWarnings(location, warningsLibrary).toArray(Warning[]::new);
    } else {
      return warnings.toArray(Warning[]::new);
    }
  }

  @ExportMessage
  Object removeWarnings(@CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    if (warnings.hasWarnings(value)) {
      return warnings.removeWarnings(value);
    } else {
      return value;
    }
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }
}
