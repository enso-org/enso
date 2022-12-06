package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Number;
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

  @Specialization
  boolean doLongCheck(long value, Type type) {
    return isNumberType(type);
  }

  @Specialization
  boolean doDoubleCheck(double value, Type type) {
    return isNumberType(type);
  }

  @Specialization
  boolean doBigIntegerCheck(EnsoBigInteger value, Type type) {
    return isNumberType(type);
  }


  private boolean isNumberType(Type type) {
    var numbers = EnsoContext.get(this).getBuiltins().number();
    if (type == numbers.getSmallInteger()) {
      return true;
    }
    if (type == numbers.getInteger()) {
      return true;
    }
    if (type == numbers.getBigInteger()) {
      return true;
    }
    if (type == numbers.getDecimal()) {
      return true;
    }
    if (type == numbers.getNumber()) {
      return true;
    }
    return false;
  }

  @Specialization
  boolean doTypeCheck(Object value, Type type, @CachedLibrary(limit = "10") TypesLibrary types) {
    var t = types.getType(value);
    return t == type;
  }

  @Fallback
  boolean doAny(Object value, Object type) {
    return value == type;
  }
}
