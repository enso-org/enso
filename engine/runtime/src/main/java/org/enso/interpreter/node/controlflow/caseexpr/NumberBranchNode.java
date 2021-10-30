package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.builtin.Number;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@NodeInfo(shortName = "NumberMatch", description = "Allows matching on the Number type.")
public abstract class NumberBranchNode extends BranchNode {
  private final AtomConstructor number;
  private final AtomConstructor integer;
  private final AtomConstructor bigInteger;
  private final AtomConstructor smallInteger;
  private final AtomConstructor decimal;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  NumberBranchNode(Number number, RootCallTarget branch) {
    super(branch);
    this.number = number.getNumber();
    this.integer = number.getInteger();
    this.bigInteger = number.getBigInteger();
    this.smallInteger = number.getSmallInteger();
    this.decimal = number.getDecimal();
  }

  /**
   * Create a new node to handle matching with the Number constructor.
   *
   * @param number the constructor used for matching
   * @param branch the code to execute
   * @return an integer branch node
   */
  public static NumberBranchNode build(Number number, RootCallTarget branch) {
    return NumberBranchNodeGen.create(number, branch);
  }

  @Specialization
  void doConstructor(VirtualFrame frame, Object state, Atom target) {
    var shouldMatch =
        (target.getConstructor() == number)
            || (target.getConstructor() == integer)
            || (target.getConstructor() == bigInteger)
            || (target.getConstructor() == smallInteger)
            || (target.getConstructor() == decimal);
    if (profile.profile(shouldMatch)) {
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

  @Specialization
  void doDecimal(VirtualFrame frame, Object state, double target) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
