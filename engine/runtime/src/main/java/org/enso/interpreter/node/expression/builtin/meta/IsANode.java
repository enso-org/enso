package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Meta",
    name = "is_a",
    description = "Checks type of a node.",
    autoRegister = false)
public abstract class IsANode extends Node {

  public abstract boolean execute(@AcceptsError Object value, Object type);

  public static IsANode build() {
    return IsANodeGen.create();
  }

  boolean isAnyType(Type t) {
    var ctx = EnsoContext.get(this);
    return ctx.getBuiltins().any() == t;
  }

  @Specialization(guards = "isAnyType(anyType)")
  boolean doAnyCheck(Object any, Type anyType) {
    return true;
  }

  @Specialization
  boolean doLongCheck(long value, Type type) {
    var numbers = EnsoContext.get(this).getBuiltins().number();
    return checkParentTypes(numbers.getSmallInteger(), type);
  }

  @Specialization
  boolean doDoubleCheck(double value, Type type) {
    var numbers = EnsoContext.get(this).getBuiltins().number();
    return checkParentTypes(numbers.getDecimal(), type);
  }

  @Specialization
  boolean doBigIntegerCheck(EnsoBigInteger value, Type type) {
    var numbers = EnsoContext.get(this).getBuiltins().number();
    return checkParentTypes(numbers.getBigInteger(), type);
  }

  @ExplodeLoop
  private boolean checkParentTypes(Type actual, Type real) {
    for (;;) {
      if (actual == null) {
        return false;
      }
      if (actual == real) {
        return true;
      }
      actual = actual.getSupertype();
    }
  }

  @Specialization
  boolean doTypeCheck(Object value, Type type, @CachedLibrary(limit = "10") TypesLibrary types) {
    var t = types.getType(value);
    return t == type;
  }

  @Fallback
  boolean doNegativeCheck(Object value, Object type) {
    return false;
  }
}
