package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "bit_xor", description = "Bitwise exclusive or.")
public abstract class BitXorNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.create();

  abstract Object execute(Object self, Object that);

  static BitXorNode build() {
    return BitXorNodeGen.create();
  }

  @Specialization
  long doLong(long self, long that) {
    return self ^ that;
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitXor(self, that.getValue()));
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitXor(self.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitXor(self.getValue(), that.getValue()));
  }

  @Fallback
  Object doOther(Object self, Object that) {
    throw IntegerUtils.throwTypeErrorIfNotInt(self, that, this);
  }
}
