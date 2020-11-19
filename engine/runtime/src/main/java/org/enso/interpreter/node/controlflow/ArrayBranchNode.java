package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;

@NodeInfo(shortName = "ArrayMatch", description = "Allows matching on the Array type.")
public abstract class ArrayBranchNode extends BranchNode {
  private final AtomConstructor array;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  ArrayBranchNode(AtomConstructor array, RootCallTarget branch) {
    super(branch);
    this.array = array;
  }

  /**
   * Create a new node to handle matching with the Array constructor.
   *
   * @param array the constructor used for matching in this case
   * @param branch the code to execute in this case
   * @return an array branch node
   */
  public static ArrayBranchNode build(AtomConstructor array, RootCallTarget branch) {
    return ArrayBranchNodeGen.create(array, branch);
  }

  @Specialization
  void doConstructor(VirtualFrame frame, Object state, Atom target) {
    if (profile.profile(array == target.getConstructor())) {
      accept(frame, state, target.getFields());
    }
  }

  @Specialization
  void doArray(VirtualFrame frame, Object state, Array target) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
