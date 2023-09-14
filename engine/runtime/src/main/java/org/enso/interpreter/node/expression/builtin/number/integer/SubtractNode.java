package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "-", description = "Subtraction of numbers.")
public abstract class SubtractNode extends IntegerNode {

  abstract Object execute(Object self, Object that);

  static SubtractNode build() {
    return SubtractNodeGen.create();
  }

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long self, long that) {
    return Math.subtractExact(self, that);
  }

  @Specialization
  Object doOverflow(long self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self, that));
  }

  @Specialization
  double doDouble(long self, double that) {
    return self - that;
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self, that.getValue()));
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.subtract(self.getValue(), that.getValue()));
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) - that;
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  Object doInterop(
      Object self,
      TruffleObject that,
      @CachedLibrary(limit = "3") InteropLibrary iop,
      @Cached SubtractNode delegate) {
    return super.doInterop(self, that, iop, delegate);
  }

  @Fallback
  Object doOther(Object self, Object that) {
    throw throwTypeErrorIfNotInt(self, that);
  }
}
