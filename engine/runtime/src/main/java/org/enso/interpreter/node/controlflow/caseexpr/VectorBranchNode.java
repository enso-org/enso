package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Vector;

/** An implementation of the case expression specialised to working on vectors. */
@NodeInfo(shortName = "VectorMatch")
public abstract class VectorBranchNode extends BranchNode {
  private final AtomConstructor vector;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  VectorBranchNode(AtomConstructor vector, RootCallTarget branch) {
    super(branch);
    this.vector = vector;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param vector the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static VectorBranchNode build(AtomConstructor vector, RootCallTarget branch) {
    return VectorBranchNodeGen.create(vector, branch);
  }

  @Specialization
  void doAtom(VirtualFrame frame, Object state, Atom target) {
    if (profile.profile(vector == target.getConstructor())) {
      accept(frame, state, target.getFields());
    }
  }

  @Specialization
  void doVector(VirtualFrame frame, Object state, Vector vector) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
