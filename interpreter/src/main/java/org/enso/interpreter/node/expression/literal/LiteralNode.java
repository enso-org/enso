package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

@NodeInfo(shortName = "Literal", description = "A representation of literal values.")
public abstract class LiteralNode extends ExpressionNode {}
