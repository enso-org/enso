package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.Vector;

/** An implementation of the case expression specialised to working on vectors. */
@NodeInfo(shortName = "VectorMatch")
public abstract class VectorBranchNode extends BranchNode {
  private final Type vector;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  VectorBranchNode(Type vector, RootCallTarget branch) {
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
  public static VectorBranchNode build(Type vector, RootCallTarget branch) {
    return VectorBranchNodeGen.create(vector, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(vector == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doVector(VirtualFrame frame, Object state, Vector vector) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
