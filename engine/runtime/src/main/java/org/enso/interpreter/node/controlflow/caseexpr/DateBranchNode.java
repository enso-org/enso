package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.EnsoDate;

/** An implementation of the case expression specialised to working on Date. */
@NodeInfo(shortName = "DateMatch")
public abstract class DateBranchNode extends BranchNode {
  private final Type date;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  DateBranchNode(Type vector, RootCallTarget branch) {
    super(branch);
    this.date = vector;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param date the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static DateBranchNode build(Type date, RootCallTarget branch) {
    return DateBranchNodeGen.create(date, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(date == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doEnsoDate(VirtualFrame frame, Object state, EnsoDate date) {
    accept(frame, state, new Object[0]);
  }

  @Specialization(guards = {"interop.isDate(date)", "!interop.isTime(date)"})
  void doDate(
      VirtualFrame frame,
      Object state,
      Object date,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
