package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.dsl.BuiltinType;

@BuiltinType(name = "Standard.Base.Any.Any")
public final class Any extends Builtin {
  public Any() {
    super(TruffleObject.class);
  }

  @Override
  public Class<? extends Builtin> getSuperType() {
    return null;
  }

  @Override
  public boolean containsValues() {
    return true;
  }
}
