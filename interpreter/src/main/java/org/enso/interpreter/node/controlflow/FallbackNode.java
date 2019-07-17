package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.function.CallNode;
import org.enso.interpreter.node.function.CallNodeGen;
import org.enso.interpreter.runtime.Atom;
import org.enso.interpreter.runtime.Function;

public class FallbackNode extends CaseNode {
  @Child private ExpressionNode functionNode;
  @Child private CallNode callNode = CallNodeGen.create();

  public FallbackNode(ExpressionNode functionNode) {
    this.functionNode = functionNode;
  }

  @Override
  public void execute(VirtualFrame frame, Atom target) throws UnexpectedResultException {
    Function function = functionNode.executeFunction(frame);
    throw new BranchSelectedException(callNode.executeCall(function, new Object[0]));
  }
}
