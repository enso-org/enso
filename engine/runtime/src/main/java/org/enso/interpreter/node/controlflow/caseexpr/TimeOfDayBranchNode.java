package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.EnsoTimeOfDay;
import org.enso.interpreter.runtime.data.Type;

/** An implementation of the case expression specialised to working on Time_Of_Day. */
@NodeInfo(shortName = "TimeOfDayMatch")
public abstract class TimeOfDayBranchNode extends BranchNode {
  private final Type timeOfDay;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  TimeOfDayBranchNode(Type vector, RootCallTarget branch) {
    super(branch);
    this.timeOfDay = vector;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param timeOfDay the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static TimeOfDayBranchNode build(Type timeOfDay, RootCallTarget branch) {
    return TimeOfDayBranchNodeGen.create(timeOfDay, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(timeOfDay == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doEnsoTimeOfDay(VirtualFrame frame, Object state, EnsoTimeOfDay timeOfDay) {
    accept(frame, state, new Object[0]);
  }

  @Specialization(guards = {"!interop.isDate(timeOfDay)", "interop.isTime(timeOfDay)"})
  void doTime(
      VirtualFrame frame,
      Object state,
      Object timeOfDay,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
