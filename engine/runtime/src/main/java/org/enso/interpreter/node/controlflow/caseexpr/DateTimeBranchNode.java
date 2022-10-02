package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.EnsoDateTime;
import org.enso.interpreter.runtime.data.Type;

/** An implementation of the case expression specialised to working on Date_Time. */
@NodeInfo(shortName = "DateTimeMatch")
public abstract class DateTimeBranchNode extends BranchNode {
  private final Type dateTime;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  DateTimeBranchNode(Type vector, RootCallTarget branch) {
    super(branch);
    this.dateTime = vector;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param dateTime the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static DateTimeBranchNode build(Type dateTime, RootCallTarget branch) {
    return DateTimeBranchNodeGen.create(dateTime, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(dateTime == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doEnsoDateTime(VirtualFrame frame, Object state, EnsoDateTime dateTime) {
    accept(frame, state, new Object[0]);
  }

  @Specialization(
      guards = {
        "interop.isDate(dateTime)",
        "interop.isTime(dateTime)",
      })
  void doDateTime(
      VirtualFrame frame,
      Object state,
      Object dateTime,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
