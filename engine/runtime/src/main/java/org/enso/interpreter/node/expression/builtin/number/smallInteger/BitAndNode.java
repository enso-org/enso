package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "bit_and", description = "Bitwise and.")
public abstract class BitAndNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  abstract Object execute(Object self, Object that);

  static BitAndNode build() {
    return BitAndNodeGen.create();
  }

  @Specialization
  long doLong(long self, long that) {
    return self & that;
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitAnd(self, that.getValue()));
  }

  @Fallback
  Object doOther(Object self, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    var integer = builtins.number().getInteger();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }
}
