package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;

@NodeInfo(shortName = "ArrayMatch", description = "Allows matching on the Array type.")
public abstract class ArrayBranchNode extends BranchNode {
  private final Type array;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  ArrayBranchNode(Type array, RootCallTarget branch) {
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
  public static ArrayBranchNode build(Type array, RootCallTarget branch) {
    return ArrayBranchNodeGen.create(array, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(array == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doArray(VirtualFrame frame, Object state, Array target) {
    accept(frame, state, new Object[0]);
  }

  @Specialization(guards = "arrays.hasArrayElements(target)")
  void doPolyglotArray(
      VirtualFrame frame,
      Object state,
      Object target,
      @CachedLibrary(limit = "5") InteropLibrary arrays) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
