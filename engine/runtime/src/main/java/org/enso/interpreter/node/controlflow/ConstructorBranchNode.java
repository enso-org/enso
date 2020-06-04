package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.node.callable.function.CreateFunctionNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.type.TypesGen;

/** An implementation of the case expression specialised to working on constructors. */
@NodeInfo(shortName = "ConstructorMatch")
public abstract class ConstructorBranchNode extends BranchNode {
  private final AtomConstructor matcher;
  private @Child ExpressionNode branch;
  private @Child ExecuteCallNode executeCallNode = ExecuteCallNodeGen.create();
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  ConstructorBranchNode(AtomConstructor matcher, CreateFunctionNode branch) {
    this.matcher = matcher;
    this.branch = branch;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matcher the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static ConstructorBranchNode build(AtomConstructor matcher, CreateFunctionNode branch) {
    return ConstructorBranchNodeGen.create(matcher, branch);
  }

  /**
   * Handles the atom scrutinee case.
   *
   * <p>The atom's constructor is checked and if it matches the conditional branch is executed with
   * all the atom's fields as arguments.
   *
   * @param frame the stack frame in which to execute
   * @param target the atom to destructure
   */
  @Specialization
  public void doAtom(VirtualFrame frame, Atom target) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    if (profile.profile(matcher == target.getConstructor())) {
      Function function = TypesGen.asFunction(branch.executeGeneric(frame));

      // Note [Caller Info For Case Branches]
      throw new BranchSelectedException(
          executeCallNode.executeCall(function, null, state, target.getFields()));
    }
  }

  /**
   * The fallback specialisation for executing the constructor branch node.
   *
   * @param frame the stack frame in which to execute
   * @param target the object to execute on
   */
  @Fallback
  public void doFallback(VirtualFrame frame, Object target) {}

  /* Note [Caller Info For Case Branches]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * It is assumed that functions serving as pattern match logic branches are always function
   * literals, not references, curried functions etc. Therefore, as function literals, they
   * have no way of accessing the caller frame and can safely be passed null.
   */
}
