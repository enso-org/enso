package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.EnsoTimeZone;
import org.enso.interpreter.runtime.data.Type;

/** An implementation of the case expression specialised to working on Time_Zone. */
@NodeInfo(shortName = "TimeZoneMatch")
public abstract class TimeZoneBranchNode extends BranchNode {
  private final Type timeZone;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  TimeZoneBranchNode(Type vector, RootCallTarget branch) {
    super(branch);
    this.timeZone = vector;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param timeZone the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static TimeZoneBranchNode build(Type timeZone, RootCallTarget branch) {
    return TimeZoneBranchNodeGen.create(timeZone, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(timeZone == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doEnsoZone(VirtualFrame frame, Object state, EnsoTimeZone timeZone) {
    accept(frame, state, new Object[0]);
  }

  @Specialization(
      guards = {"!interop.isDate(zone)", "!interop.isTime(zone)", "interop.isTimeZone(zone)"})
  void doZone(
      VirtualFrame frame,
      Object state,
      Object zone,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
