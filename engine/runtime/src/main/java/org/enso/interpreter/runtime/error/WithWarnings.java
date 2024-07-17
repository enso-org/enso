package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.callable.InteropMethodCallNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNodeGen;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;
import org.graalvm.collections.Equivalence;

/**
 * Represents a typical Enso <em>value with warnings</em>. As much of care as possible is taken to
 * delegate all operations to the underlaying {@code value}. Warnings are considered {@link
 * InteropLibrary#isException exceptional values} - e.g. one can check for them in Java polyglot
 * code as:
 *
 * <pre>
 *  Value value = ...;
 *  if (value.fitsInLong() && value.isException()) {
 *    // probably an Integer with a warning
 *    try {
 *      warningMulti.throwException();
 *    } catch (PolyglotException ex) {
 *      System.out.println("Warnings attached to " + value.asLong() + " are " + ex.getMessage());
 *    }
 *  }
 * </pre>
 */
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(WarningsLibrary.class)
@ExportLibrary(ReflectionLibrary.class)
@ExportLibrary(value = InteropLibrary.class, delegateTo = "value")
public final class WithWarnings implements EnsoObject {
  final Object value;
  private final EnsoHashMap warnings;

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
  private WithWarnings(
      Object value,
      int maxWarnings,
      boolean limitReached,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... warnings) {
    assert isAcceptableValue(value);
    this.warnings = createHashSetFromArray(maxWarnings, warnings, insertNode, interop);
    assert this.warnings.getHashSize() > 0;
    this.value = value;
    this.limitReached = limitReached || this.warnings.getHashSize() >= maxWarnings;
    this.maxWarnings = maxWarnings;
  }

  private WithWarnings(
      Object value,
      int maxWarnings,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... warnings) {
    this(value, maxWarnings, false, insertNode, interop, warnings);
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
   * @param insertNode hash map insert node used to insert warnings to the internal hash set
   * @param interop Dispatched interop library, or uncached
   * @param additionalWarnings additional warnings to be appended to the list of `warnings`
   */
  private WithWarnings(
      Object value,
      int maxWarnings,
      EnsoHashMap warnings,
      boolean limitReached,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... additionalWarnings) {
    assert isAcceptableValue(value);
    this.warnings =
        cloneSetAndAppend(maxWarnings, warnings, additionalWarnings, insertNode, interop);
    assert this.warnings.getHashSize() > 0;
    this.value = value;
    this.limitReached = limitReached || this.warnings.getHashSize() >= maxWarnings;
    this.maxWarnings = maxWarnings;
  }

  private WithWarnings(
      Object value,
      int maxWarnings,
      EnsoHashMap warnings,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... additionalWarnings) {
    this(value, maxWarnings, warnings, false, insertNode, interop, additionalWarnings);
  }

  private static boolean isAcceptableValue(Object value) {
    assert value != null;
    assert !(value instanceof WithWarnings) : "Trying to double wrap WithWarnings " + value;
    boolean goodValue =
        value instanceof TruffleObject
            || value instanceof Long
            || value instanceof Double
            || value instanceof Boolean;
    assert goodValue : "Unexpected value floating around " + value + " type: " + value.getClass();
    return goodValue;
  }

  public static WithWarnings wrap(Object value, Warning... warnings) {
    return wrap(
        value,
        EnsoContext.get(null),
        HashMapInsertNodeGen.getUncached(),
        InteropLibrary.getUncached(),
        warnings);
  }

  public static WithWarnings wrap(
      Object value,
      EnsoContext ctx,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... warnings) {
    if (value instanceof WithWarnings with) {
      return with.append(ctx, insertNode, interop, warnings);
    } else {
      return new WithWarnings(value, ctx.getWarningsLimit(), insertNode, interop, warnings);
    }
  }

  public Object getValue() {
    return value;
  }

  public WithWarnings append(
      EnsoContext ctx,
      boolean limitReached,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... newWarnings) {
    return new WithWarnings(
        value, ctx.getWarningsLimit(), warnings, limitReached, insertNode, interop, newWarnings);
  }

  public WithWarnings append(
      EnsoContext ctx,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... newWarnings) {
    return new WithWarnings(
        value, ctx.getWarningsLimit(), warnings, insertNode, interop, newWarnings);
  }

  public WithWarnings append(
      EnsoContext ctx,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      ArrayRope<Warning> newWarnings) {
    return new WithWarnings(
        value,
        ctx.getWarningsLimit(),
        warnings,
        insertNode,
        interop,
        newWarnings.toArray(Warning[]::new));
  }

  // Ignore the warnings cache in .value and re-fetch them using the WarningsLibrary.
  // This is only used for shouldWrap=true.
  private Warning[] getWarningsNoCache(WarningsLibrary warningsLibrary) {
    if (warningsLibrary != null && warningsLibrary.hasWarnings(value)) {
      try {
        return warningsLibrary.getWarnings(value, null, true);
      } catch (UnsupportedMessageException e) {
        throw EnsoContext.get(warningsLibrary).raiseAssertionPanic(warningsLibrary, null, e);
      }
    } else {
      return Warning.fromSetToArray(warnings);
    }
  }

  /**
   * Slow version of {@link #getWarningsArray(boolean, WarningsLibrary, HashMapInsertNode,
   * InteropLibrary)} that uses uncached version of nodes and libraries parameters.
   */
  public Warning[] getWarningsArray(boolean shouldWrap) {
    return getWarningsArray(
        shouldWrap,
        WarningsLibrary.getUncached(),
        HashMapInsertNodeGen.getUncached(),
        InteropLibrary.getUncached());
  }

  public Warning[] getWarningsArray(
      boolean shouldWrap,
      WarningsLibrary warningsLibrary,
      HashMapInsertNode insertNode,
      InteropLibrary interop) {
    Warning[] allWarnings;
    if (warningsLibrary != null && warningsLibrary.hasWarnings(value)) {
      try {
        var valueWarnings = warningsLibrary.getWarnings(value, null, shouldWrap);
        var tmp = cloneSetAndAppend(maxWarnings, warnings, valueWarnings, insertNode, interop);
        allWarnings = Warning.fromSetToArray(tmp);
      } catch (UnsupportedMessageException e) {
        throw EnsoContext.get(warningsLibrary).raiseAssertionPanic(warningsLibrary, null, e);
      }
    } else {
      allWarnings = Warning.fromSetToArray(warnings);
    }
    return allWarnings;
  }

  public ArrayRope<Warning> getReassignedWarningsAsRope(Node location, boolean shouldWrap) {
    return new ArrayRope<>(
        getReassignedWarnings(
            location,
            shouldWrap,
            null,
            HashMapInsertNodeGen.getUncached(),
            InteropLibrary.getUncached()));
  }

  public Warning[] getReassignedWarnings(
      Node location,
      boolean shouldWrap,
      WarningsLibrary warningsLibrary,
      HashMapInsertNode insertNode,
      InteropLibrary interop) {
    Warning[] warnings = getWarningsArray(shouldWrap, warningsLibrary, insertNode, interop);
    for (int i = 0; i < warnings.length; i++) {
      warnings[i] = warnings[i].reassign(location);
    }
    return warnings;
  }

  /**
   * Appends warnings to the {@code target}.
   *
   * <p>Slow version of {@link #appendTo(Object, ArrayRope, EnsoContext, HashMapInsertNode,
   * InteropLibrary)}.
   *
   * @param target
   * @param warnings
   * @return
   */
  public static WithWarnings appendTo(Object target, ArrayRope<Warning> warnings) {
    return appendTo(
        target,
        EnsoContext.get(null),
        HashMapInsertNodeGen.getUncached(),
        InteropLibrary.getUncached(),
        warnings);
  }

  /**
   * Slow version of {@link #appendTo(EnsoContext, Object, HashMapInsertNode, InteropLibrary,
   * Warning...)}.
   */
  public static WithWarnings appendTo(Object target, Warning... warnings) {
    return appendTo(
        target,
        EnsoContext.get(null),
        HashMapInsertNodeGen.getUncached(),
        InteropLibrary.getUncached(),
        new ArrayRope<>(warnings));
  }

  public static WithWarnings appendTo(Object target, boolean reachedMaxCount, Warning... warnings) {
    return appendTo(
        target,
        reachedMaxCount,
        EnsoContext.get(null),
        HashMapInsertNodeGen.getUncached(),
        InteropLibrary.getUncached(),
        warnings);
  }

  /**
   * Appends warnings to the {@code target}.
   *
   * @param ctx Context
   * @param target Target object that will have the warnings appended.
   * @param insertNode Hash map insert node used to insert warnings to the internal hash set.
   * @param interop Dispatched interop library, or uncached.
   * @param warnings Warnings to be appended to the {@code target}.
   * @return Target with appended warnings.
   */
  public static WithWarnings appendTo(
      Object target,
      EnsoContext ctx,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      ArrayRope<Warning> warnings) {
    if (target instanceof WithWarnings withWarn) {
      return withWarn.append(ctx, insertNode, interop, warnings.toArray(Warning[]::new));
    } else {
      return new WithWarnings(
          target, ctx.getWarningsLimit(), insertNode, interop, warnings.toArray(Warning[]::new));
    }
  }

  /**
   * Appends warnings to the {@code target}.
   *
   * @param target Target object that will have the warnings appended.
   * @param ctx Context
   * @param insertNode Hash map insert node used to insert warnings to the internal hash set.
   * @param interop Dispatched interop library, or uncached.
   * @param warnings Warnings to be appended to the {@code target}.
   * @return Target with appended warnings.
   */
  public static WithWarnings appendTo(
      Object target,
      EnsoContext ctx,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... warnings) {
    return appendTo(target, false, ctx, insertNode, interop, warnings);
  }

  public static WithWarnings appendTo(
      Object target,
      boolean reachedMaxCount,
      EnsoContext ctx,
      HashMapInsertNode insertNode,
      InteropLibrary interop,
      Warning... warnings) {
    if (target instanceof WithWarnings withWarn) {
      return withWarn.append(ctx, reachedMaxCount, insertNode, interop, warnings);
    } else {
      return new WithWarnings(
          target, ctx.getWarningsLimit(), reachedMaxCount, insertNode, interop, warnings);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private PanicException asException(Node where) {
    var rawWarn =
        this.getWarnings(
            where,
            false,
            WarningsLibrary.getUncached(),
            HashMapInsertNodeGen.getUncached(),
            InteropLibrary.getUncached());
    var ctx = EnsoContext.get(where);
    var scopeOfAny = ctx.getBuiltins().any().getDefinitionScope();
    var toText = UnresolvedSymbol.build("to_text", scopeOfAny);
    var node = InteropMethodCallNode.getUncached();
    var state = State.create(ctx);

    var text = Text.empty();
    for (var w : rawWarn) {
      try {
        var wText = node.execute(toText, state, new Object[] {w});
        if (wText instanceof Text t) {
          text = text.add(t);
        }
      } catch (ArityException e) {
        throw ctx.raiseAssertionPanic(where, null, e);
      }
    }
    return new PanicException(text, where);
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
      boolean shouldWrap,
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warningsLibrary,
      @Cached HashMapInsertNode insertNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    if (location != null) {
      return getReassignedWarnings(location, shouldWrap, warningsLibrary, insertNode, interop);
    } else {
      if (shouldWrap) {
        // In the wrapping case, we don't use the local cache in .values, since
        // it contains unwrapped warnings. Instead, we fetch them again.
        return getWarningsNoCache(warningsLibrary);
      } else {
        return Warning.fromSetToArray(warnings);
      }
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
  boolean hasType(@Shared("typesLib") @CachedLibrary(limit = "3") TypesLibrary types) {
    return types.hasType(value);
  }

  @ExportMessage
  Type getType(@Shared("typesLib") @CachedLibrary(limit = "3") TypesLibrary types) {
    return types.getType(value);
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  RuntimeException throwException(@Bind("$node") Node node) throws UnsupportedMessageException {
    throw asException(node);
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

  private EnsoHashMap createHashSetFromArray(
      int maxWarnings, Warning[] entries, HashMapInsertNode insertNode, InteropLibrary interop) {
    var set = EnsoHashMap.empty();
    for (Warning entry : entries) {
      if (set.getHashSize() == maxWarnings) {
        return set;
      }
      set = insertNode.execute(null, set, entry, null);
    }
    return set;
  }

  private EnsoHashMap cloneSetAndAppend(
      int maxWarnings,
      EnsoHashMap initial,
      Warning[] entries,
      HashMapInsertNode insertNode,
      InteropLibrary interop) {
    if (initial.getHashSize() == maxWarnings) {
      return initial;
    }
    var set = EnsoHashMap.empty();
    var initialVec = initial.getCachedVectorRepresentation();
    try {
      for (int i = 0; i < interop.getArraySize(initialVec); i++) {
        var entry = interop.readArrayElement(initialVec, i);
        var key = interop.readArrayElement(entry, 0);
        if (set.getHashSize() == maxWarnings) {
          return set;
        }
        set = insertNode.execute(null, set, key, null);
      }
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
    for (Warning warn : entries) {
      if (set.getHashSize() == maxWarnings) {
        return set;
      }
      set = insertNode.execute(null, set, warn, null);
    }
    return set;
  }

  @Override
  public String toString() {
    return "WithWarnings{"
        + value
        + " has "
        + warnings.getHashSize()
        + " warnings"
        + (limitReached ? " (warnings limit reached)}" : "}");
  }
}
