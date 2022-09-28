package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.expression.builtin.meta.IsSameObjectNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNodeGen;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** An implementation of the case expression specialised to working on types. */
@NodeInfo(shortName = "TypeMatch")
public abstract class CatchTypeBranchNode extends BranchNode {

  private final Type expectedType;
  private final boolean isArrayExepctedType;
  private @Child TypeOfNode typeOfNode = TypeOfNode.build();
  private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  CatchTypeBranchNode(Type tpe, RootCallTarget functionNode) {
    super(functionNode);
    this.expectedType = tpe;
    this.isArrayExepctedType = Context.get(this).getBuiltins().array() == tpe;
  }

  /**
   * Creates a node to handle the case by-type.
   *
   * @param functionNode the function to execute in this case
   * @return a catch-all node
   */
  public static CatchTypeBranchNode build(Type tpe, RootCallTarget functionNode) {
    return CatchTypeBranchNodeGen.create(tpe, functionNode);
  }

  @Specialization(
      guards = {
        "isArrayExepctedType()",
        "interop.hasArrayElements(value)",
        "!types.hasType(value)",
        "interop.hasMetaObject(value)"
      })
  void doPolyglotArray(
      VirtualFrame frame,
      Object state,
      Object value,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @CachedLibrary(limit = "3") TypesLibrary types) {
    accept(frame, state, new Object[] {value});
  }

  boolean isArrayExepctedType() {
    return isArrayExepctedType;
  }

  @Specialization
  public void doValue(VirtualFrame frame, Object state, Object target) {
    Object typeOfTarget = typeOfNode.execute(target);
    boolean test = isSameObject.execute(expectedType, typeOfTarget);
    if (profile.profile(test)) {
      accept(frame, state, new Object[] {target});
    }
  }
}
