package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

@NodeInfo(shortName = "/")
public abstract class DivideOperatorNode extends BinaryOperatorNode {

  @Specialization
  protected long divide(long left, long right) {
    return left / right;
  }
}
