package org.enso.interpreter.node.expression.builtin.runtime;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

import java.util.List;

@BuiltinType
public class Context extends Builtin {
  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("Input"), new Cons("Output"));
  }

  public AtomConstructor getInput() {
    return getConstructors()[0];
  }

  public AtomConstructor getOutput() {
    return getConstructors()[1];
  }
}
