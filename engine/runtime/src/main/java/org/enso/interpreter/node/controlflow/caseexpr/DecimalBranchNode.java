package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;

@NodeInfo(shortName = "TextMatch", description = "Allows matching on the Decimal type.")
public abstract class DecimalBranchNode extends BranchNode {
  private final Type decimal;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  DecimalBranchNode(Type decimal, RootCallTarget branch) {
    super(branch);
    this.decimal = decimal;
  }

  /**
   * Create a new node to handle matching with the Decimal constructor.
   *
   * @param decimal the constructor used for matching
   * @param branch the code to execute in this case
   * @return a decimal branch node
   */
  public static DecimalBranchNode build(Type decimal, RootCallTarget branch) {
    return DecimalBranchNodeGen.create(decimal, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(decimal == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doLiteral(VirtualFrame frame, Object state, double target) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
