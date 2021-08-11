package org.enso.interpreter.node.expression.foreign;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.error.DataflowError;

/** Performs a call into a given foreign call target. */
public class ForeignMethodCallNode extends ExpressionNode {
  private @Children ExpressionNode[] arguments;
  private @Child DirectCallNode callNode;
  private final BranchProfile[] errorProfiles;

  ForeignMethodCallNode(ExpressionNode[] arguments, CallTarget foreignCt) {
    this.arguments = arguments;
    this.callNode = DirectCallNode.create(foreignCt);

    this.errorProfiles = new BranchProfile[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      this.errorProfiles[i] = BranchProfile.create();
    }
  }

  /**
   * Creates a new instance of this node
   *
   * @param arguments expressions resulting in the computation of function arguments
   * @param foreignCt the foreign call target to call
   * @return the result of calling the foreign call target with the executed arguments
   */
  public static ForeignMethodCallNode build(ExpressionNode[] arguments, CallTarget foreignCt) {
    return new ForeignMethodCallNode(arguments, foreignCt);
  }

  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    Object[] args = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      args[i] = arguments[i].executeGeneric(frame);
      if (args[i] instanceof DataflowError) {
        errorProfiles[i].enter();
        return args[i];
      }
    }
    return callNode.call(args);
  }
}
