package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.expression.builtin.meta.IsSameObjectNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** An implementation of the case expression specialised to working on Date. */
@NodeInfo(shortName = "PolyglotSymbolTypeMatch")
public abstract class CatchPolyglotSymbolTypeBranchNode extends BranchNode {

  private final Object polyglotSymbol;
  private @Child TypeOfNode typeOfNode = TypeOfNode.build();
  private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  CatchPolyglotSymbolTypeBranchNode(Object polyglotSymbolName, RootCallTarget functionNode) {
    super(functionNode);
    this.polyglotSymbol = polyglotSymbolName;
  }

  /**
   * Creates a node to handle the by-type polyglot symbol case.
   *
   * @param functionNode the function to execute in this case
   * @return a catch-all node
   */
  public static CatchPolyglotSymbolTypeBranchNode build(
      Object polyglotSymbol, RootCallTarget functionNode) {
    return CatchPolyglotSymbolTypeBranchNodeGen.create(polyglotSymbol, functionNode);
  }

  @Specialization
  public void doPolyglotValue(VirtualFrame frame, Object state, Object target) {
    Object tpeOfTarget = typeOfNode.execute(target);
    boolean test = isSameObject.execute(polyglotSymbol, tpeOfTarget);
    if (profile.profile(test)) {
      accept(frame, state, new Object[] {target});
    }
  }
}
