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

/** An implementation of the case expression specialised to working on constructors. */
@NodeInfo(shortName = "ConstructorMatch")
public abstract class ConstructorBranchNode extends BranchNode {
  private final AtomConstructor matcher;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  ConstructorBranchNode(AtomConstructor matcher, RootCallTarget branch) {
    super(branch);
    this.matcher = matcher;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matcher the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static BranchNode build(AtomConstructor matcher, RootCallTarget branch) {
    if ("Standard.Builtins.Main.Vector_Data".equals(matcher.getQualifiedName().toString())) {
      return ConstructorBranchNodeGen.VectorConstructorNodeGen.create(branch);
    }
    return ConstructorBranchNodeGen.create(matcher, branch);
  }

  @Specialization
  void doAtom(VirtualFrame frame, Object state, Atom target) {
    if (profile.profile(matcher == target.getConstructor())) {
      accept(frame, state, target.getFields());
    }
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}

  @NodeInfo(shortName = "VectorConstructorMatch")
  abstract static class VectorConstructorNode extends BranchNode {

    VectorConstructorNode(RootCallTarget branch) {
      super(branch);
    }

    @Specialization
    void doVector(VirtualFrame frame, Object state, Vector target) {
      accept(frame, state, new Object[0]);
    }

    @Fallback
    void doFallback(VirtualFrame frame, Object state, Object target) {}
  }
}
