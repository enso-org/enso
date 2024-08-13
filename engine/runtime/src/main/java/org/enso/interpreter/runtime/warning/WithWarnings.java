package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
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
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertAllNode;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNodeGen;
import org.enso.interpreter.runtime.data.hash.HashMapSizeNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNodeGen;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNodeGen;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

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

  /**
   * Internal storage for warnings is a PE-friendly hash map. The key is a sequence ID (gathered
   * from {@link EnsoContext#nextSequenceId()} and the value is the warning itself. Note that it is
   * essential that sequenceId is the key so that the warnings are not duplicated.
   */
  final EnsoHashMap warnings;

  private final boolean limitReached;
  final int maxWarnings;

  /**
   * Creates a new instance of value wrapped in warnings.
   *
   * @param value value to be wrapped in warnings
   * @param maxWarnings maximal number of warnings allowed to be attached to the value
   * @param warningsMap warnings originally attached to a value
   * @param limitReached if `true`, indicates that `warnings` have already been limited for a
   *     custom-method, `false` otherwise
   */
  WithWarnings(Object value, int maxWarnings, boolean limitReached, EnsoHashMap warningsMap) {
    assert isAcceptableValue(value);
    var mapSizeNode = HashMapSizeNode.getUncached();
    assert mapSizeNode.execute(warningsMap) <= maxWarnings;
    if (limitReached) {
      assert mapSizeNode.execute(warningsMap) == maxWarnings;
    }
    this.value = value;
    this.maxWarnings = maxWarnings;
    this.limitReached = limitReached || mapSizeNode.execute(warningsMap) >= maxWarnings;
    this.warnings = warningsMap;
  }

  /**
   * Explicit creation of WithWarnings. Allows to set a specific {@code maxWarnings} count, which
   * cannot be achieved by using warning-handling nodes like {@link AppendWarningNode}. If {@code
   * maxWarnings} does not need to be set explicitly, use nodes to create WithWarning objects
   * instead.
   *
   * @param value value to be wrapped in warnings
   * @param maxWarnings maximal number of warnings allowed to be attached to the value
   * @param limitReached if `true`, no other warnings will be attached to the {@code value}.
   * @param warnings array of warnings to be attached to the {@code value}
   */
  public static WithWarnings create(
      Object value, int maxWarnings, boolean limitReached, Warning[] warnings) {
    assert warnings.length <= maxWarnings;
    if (limitReached) {
      assert warnings.length == maxWarnings;
    }
    var warnMap = Warning.fromArrayToMap(warnings, HashMapInsertNodeGen.getUncached());
    return new WithWarnings(value, maxWarnings, limitReached, warnMap);
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

  public Object getValue() {
    return value;
  }

  // Ignore the warnings cache in .value and re-fetch them using the WarningsLibrary.
  // This is only used for shouldWrap=true.
  private EnsoHashMap getWarningsNoCache(
      WarningsLibrary warningsLibrary, ArrayLikeLengthNode lengthNode, ArrayLikeAtNode atNode) {
    assert warningsLibrary != null;
    if (warningsLibrary.hasWarnings(value)) {
      try {
        return warningsLibrary.getWarnings(value, true);
      } catch (UnsupportedMessageException e) {
        throw EnsoContext.get(warningsLibrary).raiseAssertionPanic(warningsLibrary, null, e);
      }
    } else {
      return warnings;
    }
  }

  /**
   * Slow version of {@link #getWarningsArray(boolean, WarningsLibrary, HashMapInsertAllNode,
   * InteropLibrary)} that uses uncached version of nodes and libraries parameters.
   */
  public Warning[] getWarningsArray(boolean shouldWrap) {
    return getWarningsArray(
        shouldWrap,
        WarningsLibrary.getUncached(),
        HashMapInsertAllNode.getUncached(),
        InteropLibrary.getUncached());
  }

  public Warning[] getWarningsArray(
      boolean shouldWrap,
      WarningsLibrary warningsLibrary,
      HashMapInsertAllNode mapInsertAllNode,
      InteropLibrary interop) {
    Warning[] allWarnsArray;
    if (warningsLibrary != null && warningsLibrary.hasWarnings(value)) {
      try {
        var valueWarnings = warningsLibrary.getWarnings(value, shouldWrap);
        var allWarns =
            mapInsertAllNode.executeInsertAll(null, warnings, valueWarnings, maxWarnings);
        allWarnsArray = Warning.fromMapToArray(allWarns, interop);
      } catch (UnsupportedMessageException e) {
        throw EnsoContext.get(warningsLibrary).raiseAssertionPanic(warningsLibrary, null, e);
      }
    } else {
      allWarnsArray = Warning.fromMapToArray(warnings, interop);
    }
    return allWarnsArray;
  }

  @CompilerDirectives.TruffleBoundary
  private PanicException asException(Node where) {
    var warnsMap =
        this.getWarnings(
            false,
            WarningsLibrary.getUncached(),
            ArrayLikeAtNodeGen.getUncached(),
            ArrayLikeLengthNodeGen.getUncached());
    var warns = Warning.fromMapToArray(warnsMap);
    var ctx = EnsoContext.get(where);
    var scopeOfAny = ctx.getBuiltins().any().getDefinitionScope();
    var toText = UnresolvedSymbol.build("to_text", scopeOfAny);
    var node = InteropMethodCallNode.getUncached();
    var state = State.create(ctx);

    var text = Text.empty();
    for (var w : warns) {
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
  EnsoHashMap getWarnings(
      boolean shouldWrap,
      @Shared("warnsLib") @CachedLibrary(limit = "3") WarningsLibrary warningsLibrary,
      @Cached ArrayLikeAtNode atNode,
      @Cached ArrayLikeLengthNode lengthNode) {
    if (shouldWrap) {
      // In the wrapping case, we don't use the local cache in .values, since
      // it contains unwrapped warnings. Instead, we fetch them again.
      return getWarningsNoCache(warningsLibrary, lengthNode, atNode);
    } else {
      return warnings;
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
  RuntimeException throwException(@Bind("$node") Node node) {
    throw asException(node);
  }

  @Override
  public String toString() {
    return "WithWarnings{"
        + value
        + " has "
        + HashMapSizeNode.getUncached().execute(warnings)
        + " warnings"
        + (limitReached ? " (warnings limit reached)}" : "}");
  }
}
