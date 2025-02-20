package org.enso.interpreter.node.expression.builtin.ordering;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

/** A hidden builtin. Only conversions with target type of Comparable are visible. */
@BuiltinType
public final class Comparable extends Builtin {

  public Comparable() {
    super(Object.class);
  }

  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(new Cons("By", List.of("value", "comparator")));
  }

  public AtomConstructor getBy() {
    return getConstructors()[0];
  }
}
