package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

@NodeInfo(
    shortName = "BinaryOperator",
    description = "A representation of generic binary operators.")
@NodeChild("leftOperand")
@NodeChild("rightOperand")
public abstract class BinaryOperatorNode extends ExpressionNode {}
