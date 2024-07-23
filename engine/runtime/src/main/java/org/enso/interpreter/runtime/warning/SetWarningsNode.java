package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;

@BuiltinMethod(
    type = "Warning",
    name = "set_array",
    description = "Sets all the warnings associated with the value.",
    autoRegister = false)
@GenerateUncached
public abstract class SetWarningsNode extends Node {

  public static SetWarningsNode build() {
    return SetWarningsNodeGen.create();
  }

  public abstract Object execute(VirtualFrame frame, @AcceptsWarning Object value, Object warnings);

  @Specialization(guards = "isEmpty(warnings, interop)")
  Object doEmpty(
      VirtualFrame frame,
      Object object,
      Object warnings,
      @Shared @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached ConditionProfile isWithWarnsProfile) {
    if (isWithWarnsProfile.profile(object instanceof WithWarnings)) {
      return ((WithWarnings) object).value;
    } else {
      return object;
    }
  }

  @Specialization(guards = "!isEmpty(warnings, interop)")
  Object doSetArray(
      VirtualFrame frame,
      Object object,
      Warning[] warnings,
      @Shared @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached ConditionProfile isWithWarnsProfile) {
    var warnMap = EnsoHashMap.empty();
    for (var warn : warnings) {
      warnMap = mapInsertNode.execute(frame, warnMap, warn.getSequenceId(), warn);
    }
    var maxWarns = EnsoContext.get(this).getWarningsLimit();
    var isLimitReached = warnings.length >= maxWarns;
    if (isWithWarnsProfile.profile(object instanceof WithWarnings)) {
      return new WithWarnings(((WithWarnings) object).value, maxWarns, isLimitReached, warnMap);
    } else {
      return new WithWarnings(object, maxWarns, isLimitReached, warnMap);
    }
  }

  @Specialization(guards = {"!isEmpty(warnings, interop)", "!isWarnArray(warnings)"})
  Object doSetInteropArray(
      VirtualFrame frame,
      Object object,
      Object warnings,
      @Shared @CachedLibrary(limit = "3") InteropLibrary interop,
      @Shared @Cached HashMapInsertNode mapInsertNode,
      @Shared @Cached ConditionProfile isWithWarnsProfile) {
    assert !(warnings instanceof Warning[]);
    var warnMap = EnsoHashMap.empty();
    var ctx = EnsoContext.get(this);
    try {
      var size = interop.getArraySize(warnings);
      for (long i = 0; i < interop.getArraySize(warnings); i++) {
        var warn = (Warning) interop.readArrayElement(warnings, i);
        warnMap = mapInsertNode.execute(frame, warnMap, warn.getSequenceId(), warn);
      }
      var maxWarns = ctx.getWarningsLimit();
      var isLimitReached = size >= maxWarns;
      if (isWithWarnsProfile.profile(object instanceof WithWarnings)) {
        return new WithWarnings(((WithWarnings) object).value, maxWarns, isLimitReached, warnMap);
      } else {
        return new WithWarnings(object, maxWarns, isLimitReached, warnMap);
      }
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
    } catch (ClassCastException e) {
      throw ctx.raiseAssertionPanic(this, "Expected Warning, got something else", e);
    }
  }

  protected static boolean isEmpty(Object warns, InteropLibrary interop) {
    if (warns instanceof Warning[] warnsArray) {
      return warnsArray.length == 0;
    }
    if (interop.hasArrayElements(warns)) {
      try {
        return interop.getArraySize(warns) == 0;
      } catch (UnsupportedMessageException e) {
        throw CompilerDirectives.shouldNotReachHere(e);
      }
    }
    return false;
  }

  protected static boolean isWarnArray(Object obj) {
    return obj instanceof Warning[];
  }
}
