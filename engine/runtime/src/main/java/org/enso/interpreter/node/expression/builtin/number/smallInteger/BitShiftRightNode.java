package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "bit_shift_r", description = "Bitwise right-shift.")
public abstract class BitShiftRightNode extends Node {
  abstract Object execute(Object _this, Object that);

  static BitShiftRightNode build() {
    return BitShiftRightNodeGen.create();
  }

  @Specialization
  Object doBigInteger(long _this, long that, @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(_this, -1L * that);
  }

  @Specialization
  Object doBigInteger(
      long _this, EnsoBigInteger that, @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(_this, new EnsoBigInteger(BigIntegerOps.negate(that.getValue())));
  }

  @Specialization
  Object doAtomThis(Atom _this, Object that, @CachedContext(Language.class) Context ctx) {
    Builtins builtins = ctx.getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, _this, "this"), this);
  }

  @Fallback
  Object doOther(Object _this, Object that) {
    Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
    Atom integer = builtins.number().getInteger().newInstance();
    throw new PanicException(builtins.error().makeTypeError(integer, that, "that"), this);
  }
}
