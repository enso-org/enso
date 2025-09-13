package org.enso.interpreter.node.expression.builtin.text;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(name = "Standard.Base.Data.Text.Text")
public class Text extends Builtin {
  public Text() {
    super(org.enso.interpreter.runtime.data.text.Text.class);
  }

  @Override
  public boolean containsValues() {
    return true;
  }
}
