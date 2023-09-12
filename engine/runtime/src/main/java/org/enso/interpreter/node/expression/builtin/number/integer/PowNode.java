package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "^", description = "Exponentiation of numbers.")
public abstract class PowNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.create();
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
    return doBigInteger(toBigInteger(self), that);
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
    if (that.getValue().signum() > 0) {
      return Math.pow(
          BigIntegerOps.toDouble(self.getValue()), BigIntegerOps.toDouble(that.getValue()));
    } else if (that.getValue().signum() == 0) {
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

  @Fallback
  Object doOther(Object self, Object that) {
    throw IntegerUtils.throwTypeErrorIfNotInt(self, that, this);
  }
}
