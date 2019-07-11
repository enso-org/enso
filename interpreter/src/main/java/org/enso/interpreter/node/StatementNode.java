package org.enso.interpreter.node;

import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;

@NodeInfo(shortName = "EnsoStatement", description = "The base node for all enso statements.")
@ReportPolymorphism
public abstract class StatementNode extends Node {
  public abstract void execute(VirtualFrame frame);
}
