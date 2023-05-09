package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.graalvm.collections.EconomicSet;
import org.graalvm.collections.Equivalence;

import java.util.Arrays;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
@ExportLibrary(ReflectionLibrary.class)
public final class WithWarnings implements TruffleObject {

  public static final int MAX_WARNINGS_LIMIT = 100;

  private final EconomicSet<Warning> warnings;
  private final Object value;

  private final boolean reachedMax;

  private WithWarnings(Object value, boolean reachedMaxCount, Warning... warnings) {
    assert !(value instanceof WithWarnings);
    this.warnings = createSetFromArray(warnings);
    this.value = value;
    this.reachedMax = reachedMaxCount || this.warnings.size() >= MAX_WARNINGS_LIMIT;
  }
  private WithWarnings(Object value, Warning... warnings) {
    this(value, false, warnings);
  }

  private WithWarnings(Object value, EconomicSet<Warning> warnings, boolean reachedMaxCount, Warning... additionalWarnings) {
    assert !(value instanceof WithWarnings);
    this.warnings = cloneSetAndAppend(warnings, additionalWarnings);
    this.value = value;
    this.reachedMax = reachedMaxCount || this.warnings.size() >= MAX_WARNINGS_LIMIT;
  }

  private WithWarnings(Object value, EconomicSet<Warning> warnings, Warning... additionalWarnings) {
    this(value, warnings, false, additionalWarnings);
  }

  public static WithWarnings wrap(Object value, Warning... warnings) {
    if (value instanceof WithWarnings with) {
      return with.append(warnings);
    } else {
      return new WithWarnings(value, warnings);
    }
  }

  public Object getValue() {
    return value;
  }

  public WithWarnings append(boolean reachedMaxCount, Warning... newWarnings) {
    return new WithWarnings(value, warnings, reachedMaxCount, newWarnings);
  }

  public WithWarnings append(Warning... newWarnings) {
    return new WithWarnings(value, warnings, newWarnings);
  }

  public WithWarnings append(ArrayRope<Warning> newWarnings) {
    return new WithWarnings(value, warnings, newWarnings.toArray(Warning[]::new));
  }

  public Warning[] getWarningsArray(WarningsLibrary warningsLibrary) {
    Warning[] allWarnings;
    if (warningsLibrary != null && warningsLibrary.hasWarnings(value)) {
      try {
        Warning[] valueWarnings = warningsLibrary.getWarnings(value, null);
        EconomicSet<Warning> tmp = cloneSetAndAppend(warnings, valueWarnings);
        allWarnings = Warning.fromSetToArray(tmp);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(e);
      }
    } else {
      allWarnings = Warning.fromSetToArray(warnings);
    }
    return allWarnings;
  }

  /** @return the number of warnings. */
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

  public static WithWarnings appendTo(Object target, ArrayRope<Warning> warnings) {
    if (target instanceof WithWarnings) {
      return ((WithWarnings) target).append(warnings.toArray(Warning[]::new));
    } else {
      return new WithWarnings(target, warnings.toArray(Warning[]::new));
    }
  }

  public static WithWarnings appendTo(Object target, Warning... warnings) {
    return appendTo(target, false, warnings);
  }

  public static WithWarnings appendTo(Object target, boolean reachedMaxCount, Warning... warnings) {
    if (target instanceof WithWarnings) {
      return ((WithWarnings) target).append(reachedMaxCount, warnings);
    } else {
      return new WithWarnings(target, reachedMaxCount, warnings);
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
      return getReassignedWarnings(location, warningsLibrary);
    } else {
      return Warning.fromSetToArray(warnings);
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
  public boolean reachedMaxWarnings() {
    return reachedMax;
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
      return (int)((Warning)o).getSequenceId();
    }
  }

  @CompilerDirectives.TruffleBoundary
  private EconomicSet<Warning> createSetFromArray(Warning[] entries) {
    EconomicSet<Warning> set = EconomicSet.create(new WarningEquivalence());
    for (int i=0; i<entries.length; i++) {
      if (set.size() >= MAX_WARNINGS_LIMIT) {
        return set;
      }
      set.add(entries[i]);
    }
    return set;
  }

  private EconomicSet<Warning> cloneSetAndAppend(EconomicSet<Warning> initial, Warning[] entries) {
    return initial.size() == MAX_WARNINGS_LIMIT ? initial : cloneSetAndAppendSlow(initial, entries);
  }

  @CompilerDirectives.TruffleBoundary
  private EconomicSet<Warning> cloneSetAndAppendSlow(EconomicSet<Warning> initial, Warning[] entries) {
    EconomicSet<Warning> set = EconomicSet.create(new WarningEquivalence());
    for (Warning warning: initial) {
      if (set.size() >= MAX_WARNINGS_LIMIT) {
        return set;
      }
      set.add(warning);
    }
    for (int i=0; i<entries.length; i++) {
      if (set.size() >= MAX_WARNINGS_LIMIT) {
        return set;
      }
      set.add(entries[i]);
    }
    return set;
  }


  @Override
  public String toString() {
    return "WithWarnings{" + value + " + " + warnings.size() + " warnings" + (reachedMax ? " (more warnings ignored)}" : "}");
  }
}
