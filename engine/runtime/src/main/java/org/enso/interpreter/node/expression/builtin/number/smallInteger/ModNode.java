package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "%", description = "Modulo division of numbers.")
public abstract class ModNode extends Node {
  abstract Object execute(long _this, Object that);

  static ModNode build() {
    return ModNodeGen.create();
  }

  @Specialization
  Object doLong(long _this, long that) {
    try {
      return _this % that;
    } catch (ArithmeticException e) {
      return DataflowError.withoutTrace(
          Context.get(this).getBuiltins().error().getDivideByZeroError(), this);
    }
  }

  @Specialization
  long doBigInteger(long _this, EnsoBigInteger that) {
    // No need to trap, as 0 is never represented as an EnsoBigInteger.
    return _this;
  }

  @Fallback
  Object doOther(long _this, Object that) {
    Builtins builtins = Context.get(this).getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }
}
