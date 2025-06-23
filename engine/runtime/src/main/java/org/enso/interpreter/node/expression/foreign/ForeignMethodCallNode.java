package org.enso.interpreter.node.expression.foreign;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.source.Source;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;

/** Performs a call into a given foreign call target. */
public final class ForeignMethodCallNode extends ExpressionNode {
  private final Source src;
  private final String[] names;
  private @Children ExpressionNode[] arguments;
  private @Child DirectCallNode callNode;
  private @Child HostValueToEnsoNode coerceNode;
  private final BranchProfile[] errorProfiles;

  ForeignMethodCallNode(Source src, String[] names, ExpressionNode[] arguments) {
    this.src = src;
    this.names = names;
    this.arguments = arguments;
    this.callNode = null;
    this.coerceNode = HostValueToEnsoNode.build();

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
  public static ForeignMethodCallNode buildDeferred(
      Source src, String[] names, ExpressionNode[] arguments) {
    return new ForeignMethodCallNode(src, names, arguments);
  }

  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    if (callNode == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      var ctx = EnsoContext.get(this);
      var foreignCt = ctx.parseInternal(src, names);
      callNode = insert(DirectCallNode.create(foreignCt));
    }
    Object[] args = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      args[i] = arguments[i].executeGeneric(frame);
      if (args[i] instanceof DataflowError) {
        errorProfiles[i].enter();
        return args[i];
      }
    }
    return coerceNode.execute(callNode.call(args));
  }
}
