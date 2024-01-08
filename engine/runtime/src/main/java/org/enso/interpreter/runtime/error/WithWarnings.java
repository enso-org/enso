package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.EconomicSet;
import org.graalvm.collections.Equivalence;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
@ExportLibrary(ReflectionLibrary.class)
public final class WithWarnings implements EnsoObject {

  private final EconomicSet<Warning> warnings;
  private final Object value;

  private final boolean limitReached;
  private final int maxWarnings;

  /**
   * Creates a new instance of value wrapped in warnings. `limitReached` parameter allows for
   * indicating if some custom warnings filtering on `warnings` have already been performed.
   *
   * @param value value to be wrapped in warnings
   * @param maxWarnings maximal number of warnings allowed to be attached to the value
   * @param limitReached if `true`, indicates that `warnings` have already been limited for a
   *     custom-method, `false` otherwise
   * @param warnings non-empty warnings to be attached to a value
   */
  private WithWarnings(Object value, int maxWarnings, boolean limitReached, Warning... warnings) {
    assert !(value instanceof WithWarnings);
    this.warnings = createSetFromArray(maxWarnings, warnings);
    assert this.warnings.size() > 0;
    this.value = value;
    this.limitReached = limitReached || this.warnings.size() >= maxWarnings;
    this.maxWarnings = maxWarnings;
  }

  private WithWarnings(Object value, int maxWarnings, Warning... warnings) {
    this(value, maxWarnings, false, warnings);
  }

  /**
   * Creates a new instance of value wrapped in warnings. `limitReached` parameter allows for
   * indicating if some custom warnings filtering on `additionalWarnings` have already been
   * performed.
   *
   * @param value value to be wrapped in warnings
   * @param maxWarnings maximal number of warnings allowed to be attached to the value
   * @param warnings warnings originally attached to a value
   * @param limitReached if `true`, indicates that `warnings` have already been limited for a
   *     custom-method, `false` otherwise
   * @param additionalWarnings additional warnings to be appended to the list of `warnings`
   */
  private WithWarnings(
      Object value,
      int maxWarnings,
      EconomicSet<Warning> warnings,
      boolean limitReached,
      Warning... additionalWarnings) {
    assert !(value instanceof WithWarnings);
    this.warnings = cloneSetAndAppend(maxWarnings, warnings, additionalWarnings);
    assert this.warnings.size() > 0;
    this.value = value;
    this.limitReached = limitReached || this.warnings.size() >= maxWarnings;
    this.maxWarnings = maxWarnings;
  }

  private WithWarnings(
      Object value, int maxWarnings, EconomicSet<Warning> warnings, Warning... additionalWarnings) {
    this(value, maxWarnings, warnings, false, additionalWarnings);
  }

  public static WithWarnings wrap(EnsoContext ctx, Object value, Warning... warnings) {
    if (value instanceof WithWarnings with) {
      return with.append(ctx, warnings);
    } else {
      return new WithWarnings(value, ctx.getWarningsLimit(), warnings);
    }
  }

  public Object getValue() {
    return value;
  }

  public WithWarnings append(EnsoContext ctx, boolean limitReached, Warning... newWarnings) {
    return new WithWarnings(value, ctx.getWarningsLimit(), warnings, limitReached, newWarnings);
  }

  public WithWarnings append(EnsoContext ctx, Warning... newWarnings) {
    return new WithWarnings(value, ctx.getWarningsLimit(), warnings, newWarnings);
  }

  public WithWarnings append(EnsoContext ctx, ArrayRope<Warning> newWarnings) {
    return new WithWarnings(
        value, ctx.getWarningsLimit(), warnings, newWarnings.toArray(Warning[]::new));
  }

  public Warning[] getWarningsArray(WarningsLibrary warningsLibrary) {
    Warning[] allWarnings;
    if (warningsLibrary != null && warningsLibrary.hasWarnings(value)) {
      try {
        Warning[] valueWarnings = warningsLibrary.getWarnings(value, null);
        EconomicSet<Warning> tmp = cloneSetAndAppend(maxWarnings, warnings, valueWarnings);
        allWarnings = Warning.fromSetToArray(tmp);
      } catch (UnsupportedMessageException e) {
        throw EnsoContext.get(warningsLibrary).raiseAssertionPanic(warningsLibrary, null, e);
      }
    } else {
      allWarnings = Warning.fromSetToArray(warnings);
    }
    return allWarnings;
  }

  /**
   * @return the number of warnings.
   */
  public int getWarningsCount() {
    return warnings.size();
  }

  public ArrayRope<Warning> getReassignedWarningsAsRope(Node location) {
    return new ArrayRope<>(getReassignedWarnings(location, null));
  }

  public Warning[] getReassignedWarnings(Node location, WarningsLibrary warningsLibrary) {
    Warning[] warnings = getWarningsArray(warningsLibrary);
    for (int i = 0; i < warnings.length; i++) {
      warnings[i] = warnings[i].reassign(location);
    }
    return warnings;
  }

  public static WithWarnings appendTo(EnsoContext ctx, Object target, ArrayRope<Warning> warnings) {
    if (target instanceof WithWarnings) {
      return ((WithWarnings) target).append(ctx, warnings.toArray(Warning[]::new));
    } else {
      return new WithWarnings(target, ctx.getWarningsLimit(), warnings.toArray(Warning[]::new));
    }
  }

  public static WithWarnings appendTo(EnsoContext ctx, Object target, Warning... warnings) {
    return appendTo(ctx, target, false, warnings);
  }

  public static WithWarnings appendTo(
      EnsoContext ctx, Object target, boolean reachedMaxCount, Warning... warnings) {
    if (target instanceof WithWarnings) {
      return ((WithWarnings) target).append(ctx, reachedMaxCount, warnings);
    } else {
      return new WithWarnings(target, ctx.getWarningsLimit(), reachedMaxCount, warnings);
    }
  }

  @ExportMessage
  Object send(Message message, Object[] args, @CachedLibrary(limit = "3") ReflectionLibrary lib)
      throws Exception {
    return lib.send(value, message, args);
  }

  @ExportMessage
  boolean hasWarnings() {
    return true;
  }

  @ExportMessage
  Warning[] getWarnings(
      Node location,
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warningsLibrary) {
    if (location != null) {
      return getReassignedWarnings(location, warningsLibrary);
    } else {
      return Warning.fromSetToArray(warnings);
    }
  }

  @ExportMessage
  Object removeWarnings(@Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warnings)
      throws UnsupportedMessageException {
    if (warnings.hasWarnings(value)) {
      return warnings.removeWarnings(value);
    } else {
      return value;
    }
  }

  @ExportMessage
  public boolean isLimitReached() {
    return limitReached;
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }

  public static class WarningEquivalence extends Equivalence {

    @Override
    public boolean equals(Object a, Object b) {
      if (a instanceof Warning thisObj && b instanceof Warning thatObj) {
        return thisObj.getSequenceId() == thatObj.getSequenceId();
      }
      return false;
    }

    @Override
    public int hashCode(Object o) {
      return (int) ((Warning) o).getSequenceId();
    }
  }

  @CompilerDirectives.TruffleBoundary
  private EconomicSet<Warning> createSetFromArray(int maxWarnings, Warning[] entries) {
    EconomicSet<Warning> set = EconomicSet.create(new WarningEquivalence());
    for (int i = 0; i < entries.length; i++) {
      if (set.size() == maxWarnings) {
        return set;
      }
      set.add(entries[i]);
    }
    return set;
  }

  private EconomicSet<Warning> cloneSetAndAppend(
      int maxWarnings, EconomicSet<Warning> initial, Warning[] entries) {
    return initial.size() == maxWarnings
        ? initial
        : cloneSetAndAppendSlow(maxWarnings, initial, entries);
  }

  @CompilerDirectives.TruffleBoundary
  private EconomicSet<Warning> cloneSetAndAppendSlow(
      int maxWarnings, EconomicSet<Warning> initial, Warning[] entries) {
    EconomicSet<Warning> set = EconomicSet.create(new WarningEquivalence());
    for (Warning warning : initial) {
      if (set.size() == maxWarnings) {
        return set;
      }
      set.add(warning);
    }
    for (int i = 0; i < entries.length; i++) {
      if (set.size() == maxWarnings) {
        return set;
      }
      set.add(entries[i]);
    }
    return set;
  }

  @Override
  public String toString() {
    return "WithWarnings{"
        + value
        + " has "
        + warnings.size()
        + " warnings"
        + (limitReached ? " (warnings limit reached)}" : "}");
  }
}
