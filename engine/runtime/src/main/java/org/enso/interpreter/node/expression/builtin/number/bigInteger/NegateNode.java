package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;

import java.math.BigInteger;

@BuiltinMethod(type = "Number", name = "negate", description = "Negation for numbers.")
public class NegateNode extends Node {
  BigInteger execute(BigInteger _this) {
    return BigIntegerOps.negate(_this);
  }
}
