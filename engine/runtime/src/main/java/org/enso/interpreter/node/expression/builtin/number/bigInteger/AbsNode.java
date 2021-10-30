package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "abs", description = "Big integer absolute value.")
public class AbsNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  Object execute(EnsoBigInteger _this) {
    return toEnsoNumberNode.execute(BigIntegerOps.abs(_this.getValue()));
  }
}
