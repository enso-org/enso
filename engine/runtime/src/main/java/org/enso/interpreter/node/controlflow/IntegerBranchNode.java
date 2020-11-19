package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@NodeInfo(shortName = "IntegerMatch", description = "Allows matching on the Integer type.")
public abstract class IntegerBranchNode extends BranchNode {
  private final AtomConstructor integer;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  public IntegerBranchNode(AtomConstructor integer, RootCallTarget branch) {
    super(branch);
    this.integer = integer;
  }

  /**
   * Create a new node to handle matching with the Integer constructor.
   *
   * @param integer the constructor used for matching
   * @param branch the code to execute
   * @return an integer branch node
   */
  public static IntegerBranchNode build(AtomConstructor integer, RootCallTarget branch) {
    return IntegerBranchNodeGen.create(integer, branch);
  }

  @Specialization
  void doConstructor(VirtualFrame frame, Object state, Atom target) {
    if (profile.profile(integer == target.getConstructor())) {
      accept(frame, state, target.getFields());
    }
  }

  @Specialization
  void doSmallInteger(VirtualFrame frame, Object state, long target) {
    accept(frame, state, new Object[0]);
  }

  @Specialization
  void doBigInteger(VirtualFrame frame, Object state, EnsoBigInteger target) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
