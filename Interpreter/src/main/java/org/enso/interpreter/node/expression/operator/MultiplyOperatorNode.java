package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

@NodeInfo(shortName = "*")
public abstract class MultiplyOperatorNode extends BinaryOperatorNode {

  @Specialization
  protected long multiply(long left, long right) {
    return left * right;
  }
}
