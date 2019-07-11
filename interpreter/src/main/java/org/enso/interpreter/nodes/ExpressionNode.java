package org.enso.interpreter.nodes;

import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;

@NodeInfo(shortName = "EnsoExpression", description = "The base node for all enso expressions.")
@ReportPolymorphism
public abstract class ExpressionNode extends Node {
  public abstract Object execute(VirtualFrame frame);
}
