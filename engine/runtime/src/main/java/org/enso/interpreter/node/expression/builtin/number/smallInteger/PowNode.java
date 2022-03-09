package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.math.BigInteger;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "^", description = "Exponentiation of numbers.")
public abstract class PowNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();
  private @Child org.enso.interpreter.node.expression.builtin.number.smallInteger.MultiplyNode
      longMultiplyNode =
          org.enso.interpreter.node.expression.builtin.number.smallInteger.MultiplyNode.build();
  private @Child org.enso.interpreter.node.expression.builtin.number.bigInteger.MultiplyNode
      bigIntMultiplyNode =
          org.enso.interpreter.node.expression.builtin.number.bigInteger.MultiplyNode.build();
  private @Child org.enso.interpreter.node.expression.builtin.number.bigInteger.PowNode
      bigIntPowNode =
          org.enso.interpreter.node.expression.builtin.number.bigInteger.PowNode.build();

  abstract Object execute(long _this, Object that);

  static PowNode build() {
    return PowNodeGen.create();
  }

  @Specialization
  Object doLong(long _this, long that) {
    if (that < 0) {
      return Math.pow(_this, that);
    } else if (that == 0) {
      return 1L;
    } else {
      Object res = 1L;
      Object base = _this;
      while (that > 0) {
        if (that % 2 == 0) {
          if (base instanceof Long) {
            base = longMultiplyNode.execute((long) base, base);
          } else {
            base = bigIntMultiplyNode.execute((EnsoBigInteger) base, base);
          }
          that /= 2;
        } else {
          if (res instanceof Long) {
            res = longMultiplyNode.execute((long) res, base);
          } else {
            res = bigIntMultiplyNode.execute((EnsoBigInteger) res, base);
          }
          that--;
        }
      }
      return res;
    }
  }

  @Specialization
  double doDouble(long _this, double that) {
    return Math.pow(_this, that);
  }

  @Specialization
  Object doBigInteger(long _this, EnsoBigInteger that) {
    return bigIntPowNode.execute(new EnsoBigInteger(BigInteger.valueOf(_this)), that);
  }

  @Fallback
  Object doOther(long _this, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    Atom number = builtins.number().getNumber().newInstance();
    throw new PanicException(builtins.error().makeTypeError(number, that, "that"), this);
  }
}
