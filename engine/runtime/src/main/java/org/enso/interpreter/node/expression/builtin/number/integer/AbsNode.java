package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "abs", description = "Absolute value of a number")
public abstract class AbsNode extends IntegerNode {

  public static AbsNode build() {
    return AbsNodeGen.create();
  }

  public abstract Object execute(Object own);

  @Specialization(rewriteOn = ArithmeticException.class)
  long doLong(long self) {
    if (self < 0) {
      return Math.negateExact(self);
    } else {
      return self;
    }
  }

  @Specialization(replaces = "doLong")
  Object doLongOverflow(long self) {
    return toEnsoNumberNode.execute(BigIntegerOps.abs(self));
  }

  @Specialization
  Object doBigInt(EnsoBigInteger self) {
    return toEnsoNumberNode.execute(BigIntegerOps.abs(self.getValue()));
  }

  @Fallback
  Object doOther(Object self) {
    throw throwTypeErrorIfNotInt(self, this);
  }
}
