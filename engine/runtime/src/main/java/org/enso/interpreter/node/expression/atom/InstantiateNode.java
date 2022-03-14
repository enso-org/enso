package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * A node instantiating a constant {@link AtomConstructor} with values computed based on the
 * children nodes.
 */
@NodeInfo(shortName = "Instantiate", description = "Instantiates a constant Atom constructor")
public class InstantiateNode extends ExpressionNode {
  private final AtomConstructor constructor;
  private @Children ExpressionNode[] argAssignments;
  private @Children ExpressionNode[] argReads;
  private @CompilationFinal(dimensions = 1) ConditionProfile[] profiles;
  private @CompilationFinal(dimensions = 1) ConditionProfile[] warningProfiles;
  private @CompilationFinal(dimensions = 1) BranchProfile[] sentinelProfiles;
  private final ConditionProfile anyWarningsProfile = ConditionProfile.createCountingProfile();

  InstantiateNode(AtomConstructor constructor, ExpressionNode[] argAssignments, ExpressionNode[] argReads) {
    this.constructor = constructor;
    this.argAssignments = argAssignments;
    this.argReads = argReads;
    this.profiles = new ConditionProfile[argAssignments.length];
    this.sentinelProfiles = new BranchProfile[argAssignments.length];
    this.warningProfiles = new ConditionProfile[argAssignments.length];
    for (int i = 0; i < argAssignments.length; ++i) {
      this.profiles[i] = ConditionProfile.createCountingProfile();
      this.sentinelProfiles[i] = BranchProfile.create();
      this.warningProfiles[i] = ConditionProfile.createCountingProfile();
    }
  }

  /**
   * Creates an instance of this node.
   *
   * @param constructor the {@link AtomConstructor} this node will be instantiating
   * @param argAssignments the expressions that evaluate and assign constructor arguments to local vars
   * @param argReads the expressions that read field values from local vars
   * @return a node that instantiates {@code constructor}
   */
  public static InstantiateNode build(
          AtomConstructor constructor,
          ExpressionNode[] argAssignments,
          ExpressionNode[] argReads) {
    return new InstantiateNode(constructor, argAssignments, argReads);
  }

  /**
   * Executes the node, by executing all its children and putting their values as fields of the
   * newly created {@link AtomConstructor} instance.
   *
   * @param frame the stack frame for execution
   * @return the newly created {@link AtomConstructor} instance.
   */
  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    Object[] argumentValues = new Object[argAssignments.length];
    boolean anyWarnings = false;
    ArrayRope<Warning> accumulatedWarnings = new ArrayRope<>();
    for (int i = 0; i < argAssignments.length; i++) {
      ConditionProfile profile = profiles[i];
      ConditionProfile warningProfile = warningProfiles[i];
      BranchProfile sentinelProfile = sentinelProfiles[i];
      argAssignments[i].executeVoid(frame);
      Object argument = argReads[i].executeGeneric(frame);
      if (profile.profile(TypesGen.isDataflowError(argument))) {
        return argument;
      } else if (warningProfile.profile(argument instanceof WithWarnings)) {
        anyWarnings = true;
        WithWarnings originalArg = (WithWarnings) argument;
        accumulatedWarnings =
            accumulatedWarnings.append(originalArg.getReassignedWarnings(this));
        argumentValues[i] = originalArg.getValue();
      } else if (TypesGen.isPanicSentinel(argument)) {
        sentinelProfile.enter();
        throw TypesGen.asPanicSentinel(argument);
      } else {
        argumentValues[i] = argument;
      }
    }
    if (anyWarningsProfile.profile(anyWarnings)) {
      return WithWarnings.appendTo(constructor.newInstance(argumentValues), accumulatedWarnings);
    } else {
      return constructor.newInstance(argumentValues);
    }
  }
}
