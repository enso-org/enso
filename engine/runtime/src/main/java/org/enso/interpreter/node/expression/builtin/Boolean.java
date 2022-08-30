package org.enso.interpreter.node.expression.builtin;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

import java.util.List;

// Note that Boolean BuiltinType cannot be moved to `.expression.builtin.bool`
// because it currently breaks a lot of code generation for builtin methods.
// The name Boolean would clash with java.lang.Boolean.
// Before moving this definition to the `bool` package, as we should, one would have to address that
// problem first.
@BuiltinType(name = "Standard.Base.Data.Boolean.Boolean")
public class Boolean extends Builtin {
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
