package org.enso.interpreter.node.expression.builtin;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

// Note that Boolean BuiltinType cannot be moved to `.expression.builtin.bool`
// because it currently breaks a lot of code generation for builtin methods.
// The name Boolean would clash with java.lang.Boolean.
// Before moving this definition to the `bool` package, as we should, one would have to address that
// problem first.
@BuiltinType(name = "Standard.Base.Data.Boolean.Boolean")
public final class Boolean extends Builtin {
  public Boolean() {
    super(java.lang.Boolean.class);
  }

  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("False"), new Cons("True"));
  }

  public AtomConstructor getFalse() {
    return getConstructors()[0];
  }

  public AtomConstructor getTrue() {
    return getConstructors()[1];
  }
}
