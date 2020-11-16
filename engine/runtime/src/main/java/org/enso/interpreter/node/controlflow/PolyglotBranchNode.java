package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "PolyglotMatch", description = "Allows matching on polyglot objects.")
public abstract class PolyglotBranchNode extends BranchNode {
  private final AtomConstructor polyglot;
  private final ConditionProfile constructorProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile polyglotProfile = ConditionProfile.createCountingProfile();
  private @Child DirectCallNode callNode;

  PolyglotBranchNode(AtomConstructor polyglot, RootCallTarget branch) {
    this.polyglot = polyglot;
    this.callNode = DirectCallNode.create(branch);
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

  /**
   * Handles the atom scrutinee case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doConstructor(VirtualFrame frame, Object state, Atom target) {
    if (constructorProfile.profile(polyglot == target.getConstructor())) {
      // Note [Caller Info For Case Branches]
      Stateful result =
          (Stateful)
              callNode.call(
                  Function.ArgumentsHelper.buildArguments(
                      frame.materialize(), state, target.getFields()));
      throw new BranchSelectedException(result);
    }
  }

  /** Handles the case of an arbitrary object.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param obj the object to check
   * @param context the language context
   */
  @Specialization(guards = "isPolyglotObject(context,obj)")
  public void doLiteral(
      VirtualFrame frame,
      Object state,
      Object obj,
      @CachedContext(Language.class) Context context) {
    if (polyglotProfile.profile(isPolyglotObject(context, obj))) {
      // Note [Caller Info For Case Branches]
      Stateful result =
          (Stateful)
              callNode.call(
                  Function.ArgumentsHelper.buildArguments(
                      frame.materialize(), state, new Object[] {}));
      throw new BranchSelectedException(result);
    }
  }

  /**
   * The fallback specialisation for executing the polyglot branch node.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Fallback
  public void doFallback(VirtualFrame frame, Object state, Object target) {}

  boolean isPolyglotObject(Context context, Object o) {
    return context.getEnvironment().isHostObject(o);
  }
}
