package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "<=", description = "Comparison of numbers.")
public abstract class LessOrEqualNode extends IntegerNode {

  @Override
  abstract Object execute(Object own, Object that);

  static LessOrEqualNode build() {
    return LessOrEqualNodeGen.create();
  }

  @Specialization
  boolean doLong(long self, long that) {
    return self <= that;
  }

  @Specialization
  boolean doDouble(long self, double that) {
    return (double) self <= that;
  }

  @Specialization
  boolean doBigInteger(long self, EnsoBigInteger that) {
    return that.getValue().signum() > 0;
  }

  @Specialization
  boolean doDouble(EnsoBigInteger self, double that) {
    return BigIntegerOps.toDouble(self.getValue()) <= that;
  }

  @Specialization
  boolean doLong(EnsoBigInteger self, long that) {
    return self.getValue().signum() < 0;
  }

  @Specialization
  boolean doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return BigIntegerOps.compare(self.getValue(), that.getValue()) <= 0;
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  Object doInterop(
      Object self,
      TruffleObject that,
      @CachedLibrary(limit = "3") InteropLibrary iop,
      @Cached LessOrEqualNode delegate) {
    return super.doInterop(self, that, iop, delegate);
  }

  @Fallback
  Object doOther(Object self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var incomparableValsErr = builtins.error().makeIncomparableValues(self, that);
    return DataflowError.withoutTrace(incomparableValsErr, this);
  }
}
