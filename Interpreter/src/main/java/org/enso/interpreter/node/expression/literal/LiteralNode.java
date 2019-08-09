package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/**
 * A base class for all Enso literals.
 *
 * <p>It exists only to enable working with literal values as an aggregate.
 */
@NodeInfo(shortName = "Literal", description = "A representation of literal values.")
public abstract class LiteralNode extends ExpressionNode {}
