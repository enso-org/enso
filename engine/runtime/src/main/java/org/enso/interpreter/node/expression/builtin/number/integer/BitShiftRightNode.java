package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "bit_shift_r", description = "Bitwise right-shift.")
public abstract class BitShiftRightNode extends IntegerNode.Binary {

  @Override
  abstract Object executeBinary(Object own, Object that);

  static BitShiftRightNode build() {
    return BitShiftRightNodeGen.create();
  }

  @Specialization
  Object doBigInteger(
      long self, long that, @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(self, -1L * that);
  }

  @Specialization
  Object doBigInteger(
      long self,
      EnsoBigInteger that,
      @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(self, new EnsoBigInteger(BigIntegerOps.negate(that.getValue())));
  }

  @Specialization
  Object doBigInteger(
      EnsoBigInteger self,
      long that,
      @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(self, -1L * that);
  }

  @Specialization
  Object doBigInteger(
      EnsoBigInteger self,
      EnsoBigInteger that,
      @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(self, new EnsoBigInteger(BigIntegerOps.negate(that.getValue())));
  }

  @Fallback
  Object doOther(Object self, Object that) {
    throw throwTypeErrorIfNotInt(self, that);
  }
}
