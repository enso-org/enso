package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.node.callable.function.CreateFunctionNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * This node represents an explicit catch-call case in a pattern match, as provided by the user. It
 * executes the catch-all case code.
 */
@NodeInfo(
    shortName = "Catch_All",
    description = "An explicit catch-all branch in a case expression")
public class CatchAllBranchNode extends BranchNode {
  @Child private ExpressionNode functionNode;
  @Child private ExecuteCallNode executeCallNode = ExecuteCallNodeGen.create();

  private CatchAllBranchNode(CreateFunctionNode functionNode) {
    this.functionNode = functionNode;
  }

  /**
   * Creates a node to handle the case catch-all.
   *
   * @param functionNode the function to execute in this case
   * @return a catch-all node
   */
  public static CatchAllBranchNode build(CreateFunctionNode functionNode) {
    return new CatchAllBranchNode(functionNode);
  }

  /**
   * Executes the case branch on an arbitrary target.
   *
   * @param frame the stack frame in which to execute
   * @param target the object to match against
   */
  public void execute(VirtualFrame frame, Object target) {
    // Note [Safe Casting to Function in Catch All Branches]
    Function function = TypesGen.asFunction(functionNode.executeGeneric(frame));
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    throw new BranchSelectedException(
        // Note [Caller Info For Case Branches]
        executeCallNode.executeCall(function, null, state, new Object[] {target}));
  }

  /* Note [Safe Casting to Function in Catch All Branches]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * The syntactic nature of a catch all node guarantees that it has _only one_
   * matcher in its pattern, regardless of whether it is named or a blank. As
   * a result, we _know_ that the expression of the branch will _always_ be a
   * function at code generation time, and hence we know that we can safely cast
   * it to a function during execution.
   */
}
