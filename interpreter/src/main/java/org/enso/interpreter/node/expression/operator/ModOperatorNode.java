package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

@NodeInfo(shortName = "%")
public abstract class ModOperatorNode extends BinaryOperatorNode {

  @Specialization
  protected long mod(long left, long right) {
    return left % right;
  }
}
