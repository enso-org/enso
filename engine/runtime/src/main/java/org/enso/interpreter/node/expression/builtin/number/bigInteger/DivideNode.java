package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "/", description = "Big integer division.")
public abstract class DivideNode extends Node {
  private @Child
  ToEnsoNumberNode toLongNode = ToEnsoNumberNode.build();

  abstract Object execute(EnsoBigInteger _this, Object that);

  static DivideNode build() {
    return DivideNodeGen.create();
  }

  @Specialization
  Object doLong(EnsoBigInteger _this, long that) {
    return toLongNode.execute(BigIntegerOps.divide(_this.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this, EnsoBigInteger that) {
    return toLongNode.execute(BigIntegerOps.divide(_this.getValue(), that.getValue()));
  }
}
