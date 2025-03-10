package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinType(name = "Standard.Base.Error.Error")
public final class Error extends Builtin {
  public Error() {
    super(DataflowError.class);
  }

  @Override
  protected Class<? extends Builtin> getSuperType() {
    return null;
  }

  @Override
  public boolean containsValues() {
    return true;
  }
}
