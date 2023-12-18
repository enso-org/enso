package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node.Child;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "^", description = "Exponentiation of numbers.")
public abstract class PowNode extends IntegerNode {

  private @Child MultiplyNode multiplyNode = MultiplyNode.build();

  abstract Object execute(Object self, Object that);

  static PowNode build() {
    return PowNodeGen.create();
  }

  @Specialization
  Object doLong(long self, long that) {
    if (that < 0) {
      return Math.pow(self, that);
    } else if (that == 0) {
      return 1L;
    } else {
      Object res = 1L;
      Object base = self;
      while (that > 0) {
        if (that % 2 == 0) {
          base = multiplyNode.execute(base, base);
          that /= 2;
        } else {
          res = multiplyNode.execute(res, base);
          that--;
        }
      }
      return res;
    }
  }

  @Specialization
  double doDouble(long self, double that) {
    return Math.pow(self, that);
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    var thatValue = that.getValue();
    if (thatValue.signum() > 0) {
      return Math.pow((double) self, BigIntegerOps.toDouble(thatValue));
    } else if (thatValue.signum() == 0) {
      return 1.0D;
    } else {
      return 0.0D;
    }
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    if (that == 0) {
      return 1L;
    } else if (that > 0) {
      return toEnsoNumberNode.execute(BigIntegerOps.pow(self.getValue(), that));
    } else {
      return Math.pow(BigIntegerOps.toDouble(self.getValue()), that);
    }
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    var thatValue = that.getValue();
    if (thatValue.signum() > 0) {
      var selfValue = self.getValue();
      return Math.pow(BigIntegerOps.toDouble(selfValue), BigIntegerOps.toDouble(thatValue));
    } else if (thatValue.signum() == 0) {
      return 1.0D;
    } else {
      return 0.0D;
    }
  }

  @Specialization
  double doDouble(EnsoBigInteger self, double that) {
    return Math.pow(BigIntegerOps.toDouble(self.getValue()), that);
  }

  @CompilerDirectives.TruffleBoundary
  private static EnsoBigInteger toBigInteger(long self) {
    return new EnsoBigInteger(BigInteger.valueOf(self));
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  Object doInterop(
      Object self,
      TruffleObject that,
      @CachedLibrary(limit = "3") InteropLibrary iop,
      @Cached PowNode delegate) {
    return super.doInterop(self, that, iop, delegate);
  }

  @Fallback
  Object doOther(Object self, Object that) {
    throw throwTypeErrorIfNotInt(self, that);
  }
}
