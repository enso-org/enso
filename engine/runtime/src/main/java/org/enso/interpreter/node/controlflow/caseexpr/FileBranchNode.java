package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.data.Type;

@NodeInfo(shortName = "FileMatch", description = "Allows matching on the File type.")
public abstract class FileBranchNode extends BranchNode {
  private final Type file;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  FileBranchNode(Type file, RootCallTarget branch) {
    super(branch);
    this.file = file;
  }

  /**
   * Create a new node to handle matching with the File constructor.
   *
   * @param file the constructor used for matching in this case
   * @param branch the code to execute in this case
   * @return a file branch node
   */
  public static FileBranchNode build(Type file, RootCallTarget branch) {
    return FileBranchNodeGen.create(file, branch);
  }

  @Specialization
  void doType(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(file == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doFile(VirtualFrame frame, Object state, EnsoFile target) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}
