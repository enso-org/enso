package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.runtime.TypesGen;

@NodeInfo(shortName = "EnsoExpression", description = "The base node for all enso expressions.")
@ReportPolymorphism
public abstract class ExpressionNode extends StatementNode {

  @CompilerDirectives.CompilationFinal private boolean isTail = false;

  public void markTail() {
    isTail = true;
  }

  public void markNotTail() {
    isTail = false;
  }

  public boolean isTail() {
    return isTail;
  }

  public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
    return TypesGen.expectLong(executeGeneric(frame));
  }
  public abstract Object executeGeneric(VirtualFrame frame);

  public void execute(VirtualFrame frame) {
    executeGeneric(frame);
  }
}
