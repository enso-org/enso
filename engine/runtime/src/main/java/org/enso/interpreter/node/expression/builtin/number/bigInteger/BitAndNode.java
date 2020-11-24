package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.TypeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "bit_and", description = "Bitwise and.")
public abstract class BitAndNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  abstract Object execute(Object _this, Object that);

  static BitAndNode build() {
    return BitAndNodeGen.create();
  }

  @Specialization
  Object doLong(EnsoBigInteger _this, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitAnd(_this.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitAnd(_this.getValue(), that.getValue()));
  }

  @Specialization
  Object doAtomThis(Atom _this, Object that) {
    throw new TypeError("Unexpected type provided for `this` in Integer.bit_and", this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    throw new TypeError("Unexpected type provided for `that` in Integer.bit_and", this);
  }
}
