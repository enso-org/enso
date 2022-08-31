package org.enso.interpreter.node.expression.builtin.immutable;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;

@BuiltinType(name = "Standard.Base.Data.Vector.Vector")
public class Vector extends Builtin {
  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("Vector_Data"));
  }
}
