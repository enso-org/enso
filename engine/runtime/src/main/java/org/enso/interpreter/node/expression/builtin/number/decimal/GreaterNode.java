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

@BuiltinMethod(type = "Float", name = ">", description = "Comparison of numbers.")
public abstract class GreaterNode extends FloatNode {

  abstract Object execute(double own, Object that);

  static GreaterNode build() {
    return GreaterNodeGen.create();
  }

  @Specialization
  Object doDouble(double self, double that) {
    if (Double.isNaN(self) || Double.isNaN(that)) {
      return incomparableError(self, that);
    } else {
      return self > that;
    }
  }

  @Specialization
  Object doLong(double self, long that) {
    if (Double.isNaN(self)) {
      return incomparableError(self, that);
    } else {
      return self > (double) that;
    }
  }

  @Specialization
  Object doBigInteger(double self, EnsoBigInteger that) {
    if (Double.isNaN(self)) {
      return incomparableError(self, that);
    } else {
      return self > BigIntegerOps.toDouble(that.getValue());
    }
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  Object doInterop(
      double self,
      TruffleObject that,
      @CachedLibrary(limit = INTEROP_LIMIT) InteropLibrary iop,
      @Cached ToEnsoNumberNode toEnsoNumberNode,
      @Cached GreaterNode delegate) {
    return delegate.execute(self, handleInterop(true, self, that, iop, toEnsoNumberNode));
  }

  @Fallback
  Object doOther(double self, Object that) {
    return incomparableError(self, that);
  }
}
