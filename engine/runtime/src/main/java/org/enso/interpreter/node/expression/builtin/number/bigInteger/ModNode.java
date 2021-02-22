package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "%", description = "Big integer modulo division.")
public abstract class ModNode extends Node {
  private @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.build();

  abstract Object execute(EnsoBigInteger _this, Object that);

  static ModNode build() {
    return ModNodeGen.create();
  }

  @Specialization
  Object doLong(
      EnsoBigInteger _this,
      long that,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    try {
      return toEnsoNumberNode.execute(BigIntegerOps.modulo(_this.getValue(), that));
    } catch (ArithmeticException e) {
      return DataflowError.withoutTrace(
          ctxRef.get().getBuiltins().error().getDivideByZeroError(), this);
    }
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger _this, EnsoBigInteger that) {
    // No need to trap, as 0 is never represented as an EnsoBigInteger.
    return toEnsoNumberNode.execute(BigIntegerOps.modulo(_this.getValue(), that.getValue()));
  }

  @Fallback
  Object doOther(EnsoBigInteger _this, Object that) {
    Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }
}
