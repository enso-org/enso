package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;

@GenerateUncached
public abstract class AppendWarningNode extends Node {

  public static AppendWarningNode build() {
    return AppendWarningNodeGen.create();
  }

  /**
   * Appends a warning to the given object.
   *
   * @param object Object that will have the warning appended
   * @param warnings Either an array-like object containing warnings to append, or a single warning.
   * @return A wrapped object with warnings
   */
  public abstract WithWarnings execute(VirtualFrame frame, Object object, Object warnings);

  @Specialization
  WithWarnings doWithWarnMultipleWarningsArray(
      VirtualFrame frame,
      WithWarnings withWarn,
      Warning[] warnings,
      @Shared @Cached HashMapInsertNode mapInsertNode) {
    var warnsMap = withWarn.warnings;
    for (var warn : warnings) {
      warnsMap = mapInsertNode.execute(frame, warnsMap, warn, null);
    }
    var currWarnsCnt = withWarn.warnings.getHashSize();
    var newWarnsCnt = warnings.length;
    var isLimitReached = currWarnsCnt + newWarnsCnt >= withWarn.maxWarnings;
    return new WithWarnings(withWarn.value, withWarn.maxWarnings, isLimitReached, warnsMap);
  }

  @Specialization
  WithWarnings doGenericMultipleWarningsArray(
      VirtualFrame frame,
      Object object,
      Warning[] warnings,
      @Shared @Cached HashMapInsertNode mapInsertNode) {
    var warnsMap = EnsoHashMap.empty();
    for (var warn : warnings) {
      warnsMap = mapInsertNode.execute(frame, warnsMap, warn, null);
    }
    var warnsLimit = EnsoContext.get(this).getWarningsLimit();
    var isLimitReached = warnings.length >= warnsLimit;
    return new WithWarnings(object, warnsLimit, isLimitReached, warnsMap);
  }

  @Specialization(guards = "interop.hasArrayElements(warnings)")
  WithWarnings doWithWarnMultipleWarningsInterop(
      VirtualFrame frame,
      WithWarnings withWarn,
      Object warnings,
      @Shared @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached ArrayLikeAtNode atNode,
      @Shared @Cached ArrayLikeLengthNode lengthNode) {
    var resWarningMap =
        insertToWarningMap(frame, withWarn.warnings, warnings, lengthNode, atNode, mapInsertNode);
    var currWarnsCnt = withWarn.warnings.getHashSize();
    var newWarnsCnt = lengthNode.executeLength(warnings);
    var isLimitReached = currWarnsCnt + newWarnsCnt >= withWarn.maxWarnings;
    return new WithWarnings(withWarn.value, withWarn.maxWarnings, isLimitReached, resWarningMap);
  }

  @Specialization(guards = {"!interop.hasArrayElements(warning)", "!isWarnArray(warning)"})
  WithWarnings doWithWarnSingleWarning(
      VirtualFrame frame,
      WithWarnings withWarn,
      Object warning,
      @Shared @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached HashMapInsertNode mapInsertNode) {
    var warningsMap = withWarn.warnings;
    var currWarnsCnt = warningsMap.getHashSize();
    var newWarnsMap = mapInsertNode.execute(frame, warningsMap, warning, null);
    var isLimitReached = currWarnsCnt + 1 >= withWarn.maxWarnings;
    return new WithWarnings(withWarn.value, withWarn.maxWarnings, isLimitReached, newWarnsMap);
  }

  @Specialization(guards = {"!interop.hasArrayElements(warning)", "!isWarnArray(warning)"})
  WithWarnings doGenericSingleWarning(
      VirtualFrame frame,
      Object value,
      Object warning,
      @Shared @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached HashMapInsertNode mapInsertNode) {
    var warnsMap = mapInsertNode.execute(frame, EnsoHashMap.empty(), warning, null);
    var warnsLimit = EnsoContext.get(this).getWarningsLimit();
    var limitReached = 1 >= warnsLimit;
    return new WithWarnings(value, warnsLimit, limitReached, warnsMap);
  }

  @Specialization(guards = "interop.hasArrayElements(warnings)")
  WithWarnings doGenericMultipleWarnings(
      VirtualFrame frame,
      Object object,
      Object warnings,
      @Shared @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached ArrayLikeLengthNode lengthNode,
      @Shared @Cached ArrayLikeAtNode atNode,
      @Shared @Cached HashMapInsertNode mapInsertNode) {
    var resWarningMap =
        insertToWarningMap(frame, EnsoHashMap.empty(), warnings, lengthNode, atNode, mapInsertNode);
    var newWarnsCnt = lengthNode.executeLength(warnings);
    var warnsLimit = EnsoContext.get(this).getWarningsLimit();
    var isLimitReached = newWarnsCnt >= warnsLimit;
    return new WithWarnings(object, warnsLimit, isLimitReached, resWarningMap);
  }

  /** Inserts all {@code warnings} to the {@code initialWarningMap}. */
  private EnsoHashMap insertToWarningMap(
      VirtualFrame frame,
      EnsoHashMap initialWarningMap,
      Object warnings,
      ArrayLikeLengthNode lengthNode,
      ArrayLikeAtNode atNode,
      HashMapInsertNode mapInsertNode) {
    EnsoHashMap resWarningMap = initialWarningMap;
    for (long i = 0; i < lengthNode.executeLength(warnings); i++) {
      Warning warn;
      try {
        warn = (Warning) atNode.executeAt(warnings, i);
      } catch (InvalidArrayIndexException e) {
        throw CompilerDirectives.shouldNotReachHere(e);
      } catch (ClassCastException e) {
        throw EnsoContext.get(this).raiseAssertionPanic(this, "Expected warning object", e);
      }
      resWarningMap = mapInsertNode.execute(frame, resWarningMap, warn, null);
    }
    return resWarningMap;
  }

  protected static boolean isWarnArray(Object obj) {
    return obj instanceof Warning[];
  }
}
