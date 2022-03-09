package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

@NodeInfo(shortName = "PolyglotMatch", description = "Allows matching on polyglot objects.")
public abstract class PolyglotBranchNode extends BranchNode {
  private final AtomConstructor polyglot;
  private final ConditionProfile constructorProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile polyglotProfile = ConditionProfile.createCountingProfile();

  PolyglotBranchNode(AtomConstructor polyglot, RootCallTarget branch) {
    super(branch);
    this.polyglot = polyglot;
  }

  /**
   * Create a new node to handle matching with the Polyglot constructor.
   *
   * @param polyglot the constructor used for matching
   * @param branch the code to execute
   * @return an integer branch node
   */
  public static PolyglotBranchNode build(AtomConstructor polyglot, RootCallTarget branch) {
    return PolyglotBranchNodeGen.create(polyglot, branch);
  }

  @Specialization
  void doConstructor(VirtualFrame frame, Object state, Atom target) {
    if (constructorProfile.profile(polyglot == target.getConstructor())) {
      accept(frame, state, target.getFields());
    }
  }

  @Specialization(guards = "isPolyglotObject(obj)")
  void doLiteral(
      VirtualFrame frame,
      Object state,
      Object obj) {
    if (polyglotProfile.profile(isPolyglotObject(obj))) {
      accept(frame, state, new Object[0]);
    }
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}

  boolean isPolyglotObject(Object o) {
    return Context.get(this).getEnvironment().isHostObject(o);
  }
}
