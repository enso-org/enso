package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;

/** An implementation of the case expression specialised to working on constructors. */
@NodeInfo(shortName = "ConstructorMatch")
public abstract class ConstructorBranchNode extends BranchNode {
  private final AtomConstructor matcher;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  ConstructorBranchNode(AtomConstructor matcher, RootCallTarget branch, boolean terminalBranch) {
    super(branch, terminalBranch);
    this.matcher = matcher;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matcher the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static ConstructorBranchNode build(
      AtomConstructor matcher, RootCallTarget branch, boolean terminalBranch) {
    return ConstructorBranchNodeGen.create(matcher, branch, terminalBranch);
  }

  @Specialization
  void doAtom(
      VirtualFrame frame,
      Object state,
      Atom target,
      @CachedLibrary(limit = "10") StructsLibrary structs) {
    if (profile.profile(matcher == target.getConstructor())) {
      accept(frame, state, structs.getFields(target));
    }
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
