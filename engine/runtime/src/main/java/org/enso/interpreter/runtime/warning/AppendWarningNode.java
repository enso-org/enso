package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertAllNode;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;
import org.enso.interpreter.runtime.data.hash.HashMapSizeNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.DataflowError;

@GenerateUncached
public abstract class AppendWarningNode extends Node {

  @NeverDefault
  public static AppendWarningNode build() {
    return AppendWarningNodeGen.create();
  }

  public static AppendWarningNode getUncached() {
    return AppendWarningNodeGen.getUncached();
  }

  /**
   * Appends a warning to the given object.
   *
   * @param object Object that will have the warning appended
   * @param warnings Either an array-like object containing warnings to append, or a single warning.
   *     It is expected that all the elements in the container are of {@link Warning} class.
   * @return A wrapped object with warnings
   */
  public abstract Object executeAppend(VirtualFrame frame, Object object, Object warnings);

  @Specialization(guards = "!isError(object)")
  WithWarnings doSingleWarning(
      VirtualFrame frame,
      Object object,
      Warning warning,
      @Shared @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached HashMapSizeNode mapSizeNode,
      @Shared @Cached ConditionProfile isWithWarnsProfile) {
    EnsoHashMap warnsMap;
    int warnsLimit;
    Object value;
    if (isWithWarnsProfile.profile(object instanceof WithWarnings)) {
      warnsMap = ((WithWarnings) object).warnings;
      value = ((WithWarnings) object).value;
      warnsLimit = ((WithWarnings) object).maxWarnings;
    } else {
      warnsMap = EnsoHashMap.empty();
      value = object;
      warnsLimit = EnsoContext.get(this).getWarningsLimit();
    }
    boolean isLimitReached;
    if ((int) mapSizeNode.execute(warnsMap) < warnsLimit) {
      warnsMap = mapInsertNode.execute(frame, warnsMap, warning.getSequenceId(), warning);
      isLimitReached = false;
    } else {
      isLimitReached = true;
    }
    return new WithWarnings(value, warnsLimit, isLimitReached, warnsMap);
  }

  @Specialization(guards = "!isError(object)")
  WithWarnings doMultipleWarningsArray(
      VirtualFrame frame,
      Object object,
      Warning[] warnings,
      @Shared @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached HashMapSizeNode mapSizeNode,
      @Shared @Cached ConditionProfile isWithWarnsProfile) {
    EnsoHashMap warnsMap;
    int warnsLimit;
    Object value;
    if (isWithWarnsProfile.profile(object instanceof WithWarnings)) {
      warnsMap = ((WithWarnings) object).warnings;
      warnsLimit = ((WithWarnings) object).maxWarnings;
      value = ((WithWarnings) object).value;
    } else {
      warnsMap = EnsoHashMap.empty();
      warnsLimit = EnsoContext.get(this).getWarningsLimit();
      value = object;
    }
    var isLimitReached = false;
    for (var warn : warnings) {
      if (mapSizeNode.execute(warnsMap) < warnsLimit) {
        warnsMap = mapInsertNode.execute(frame, warnsMap, warn.getSequenceId(), warn);
      } else {
        isLimitReached = true;
        break;
      }
    }
    return new WithWarnings(value, warnsLimit, isLimitReached, warnsMap);
  }

  /**
   * This specialization should be the most frequent - just wrapping the given {@code object} with
   * warnings hash map
   */
  @Specialization(guards = {"!isError(object)", "!isWithWarns(object)"})
  WithWarnings doObjectMultipleWarningsHashMap(
      VirtualFrame frame,
      Object object,
      EnsoHashMap newWarnsMap,
      @Shared @Cached HashMapSizeNode mapSizeNode) {
    assert !(object instanceof WithWarnings);
    int warnsLimit = EnsoContext.get(this).getWarningsLimit();
    var limitReached = mapSizeNode.execute(newWarnsMap) >= warnsLimit;
    return new WithWarnings(object, warnsLimit, limitReached, newWarnsMap);
  }

  @Specialization
  WithWarnings doWithWarnMultipleWarningsHashMap(
      VirtualFrame frame,
      WithWarnings withWarnings,
      EnsoHashMap newWarnsMap,
      @Cached HashMapInsertAllNode mapInsertAllNode,
      @Shared @Cached HashMapSizeNode mapSizeNode) {
    if (withWarnings.isLimitReached()) {
      return withWarnings;
    }
    var maxWarns = withWarnings.maxWarnings;
    var warnsMap = withWarnings.warnings;
    var curWarnsCnt = (int) mapSizeNode.execute(warnsMap);
    warnsMap = mapInsertAllNode.executeInsertAll(frame, warnsMap, newWarnsMap, maxWarns);
    var isLimitReached = mapSizeNode.execute(warnsMap) >= maxWarns;
    return new WithWarnings(withWarnings.value, withWarnings.maxWarnings, isLimitReached, warnsMap);
  }

  @Specialization(
      guards = {"interop.hasArrayElements(warnings)", "!isError(object)", "!isWarnArray(warnings)"})
  WithWarnings doMultipleWarningsInterop(
      VirtualFrame frame,
      Object object,
      Object warnings,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached HashMapInsertNode mapInsertNode,
      @Cached ArrayLikeAtNode atNode,
      @Cached ArrayLikeLengthNode lengthNode,
      @Shared @Cached HashMapSizeNode mapSizeNode,
      @Shared @Cached ConditionProfile isWithWarnsProfile) {
    assert !(warnings instanceof Warning[]);
    EnsoHashMap warnsMap;
    int warnsLimit;
    Object value;
    if (isWithWarnsProfile.profile(object instanceof WithWarnings)) {
      value = ((WithWarnings) object).value;
      warnsLimit = ((WithWarnings) object).maxWarnings;
      warnsMap = ((WithWarnings) object).warnings;
    } else {
      value = object;
      warnsLimit = EnsoContext.get(this).getWarningsLimit();
      warnsMap = EnsoHashMap.empty();
    }
    var currWarnsCnt = mapSizeNode.execute(warnsMap);
    var resWarningMap =
        insertToWarningMap(
            frame, warnsMap, warnings, warnsLimit, lengthNode, atNode, mapInsertNode, mapSizeNode);
    var newWarnsCnt = lengthNode.executeLength(warnings);
    var isLimitReached = currWarnsCnt + newWarnsCnt >= warnsLimit;
    return new WithWarnings(value, warnsLimit, isLimitReached, resWarningMap);
  }

  @Specialization(guards = "isError(object)")
  Object dontAnnotateError(Object object, Object ignoreWarnings) {
    return object;
  }

  /** Inserts all {@code warnings} to the {@code initialWarningMap}. */
  private EnsoHashMap insertToWarningMap(
      VirtualFrame frame,
      EnsoHashMap initialWarningMap,
      Object warnings,
      int warnsLimit,
      ArrayLikeLengthNode lengthNode,
      ArrayLikeAtNode atNode,
      HashMapInsertNode mapInsertNode,
      HashMapSizeNode mapSizeNode) {
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
      if (mapSizeNode.execute(resWarningMap) >= warnsLimit) {
        return resWarningMap;
      }
      resWarningMap = mapInsertNode.execute(frame, resWarningMap, warn.getSequenceId(), warn);
    }
    return resWarningMap;
  }

  static boolean isWarnArray(Object obj) {
    return obj instanceof Warning[];
  }

  static boolean isWithWarns(Object obj) {
    return obj instanceof WithWarnings;
  }

  static boolean isError(Object obj) {
    return obj instanceof DataflowError;
  }
}
