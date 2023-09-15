package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Float", name = "%", description = "Modulo division of numbers.")
public abstract class ModNode extends FloatNode {
  abstract double execute(double self, Object that);

  static ModNode build() {
    return ModNodeGen.create();
  }

  @Specialization
  double doDouble(double self, double that) {
    return self % that;
  }

  @Specialization
  double doLong(double self, long that) {
    return self % that;
  }

  @Specialization
  double doBigInteger(double self, EnsoBigInteger that) {
    return self % BigIntegerOps.toDouble(that.getValue());
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  double doInterop(
      double self,
      TruffleObject that,
      @CachedLibrary(limit = INTEROP_LIMIT) InteropLibrary iop,
      @Cached ToEnsoNumberNode toEnsoNumberNode,
      @Cached ModNode delegate) {
    return delegate.execute(self, handleInterop(false, self, that, iop, toEnsoNumberNode));
  }

  @Fallback
  double doOther(double self, Object that) {
    throw panicOtherwise(self, that);
  }
}
