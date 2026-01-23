package org.enso.interpreter.runtime.builtin;

import java.util.function.Supplier;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;
import org.enso.interpreter.runtime.util.CachingSupplier;

public final class Ordering {
  private final Supplier<Type> type;
  private final Supplier<Atom> equal;
  private final Supplier<Atom> less;
  private final Supplier<Atom> greater;

  Ordering(Supplier<Type> type) {
    this.type = type;
    this.equal = toAtom("Equal");
    this.less = toAtom("Less");
    this.greater = toAtom("Greater");
  }

  public Type getType() {
    return type.get();
  }

  public Atom newEqual() {
    return equal.get();
  }

  public Atom newLess() {
    return less.get();
  }

  public Atom newGreater() {
    return greater.get();
  }

  private Supplier<Atom> toAtom(String consName) {
    return CachingSupplier.from(
        () -> {
          var cons = getType().getConstructors().get(consName);
          return AtomNewInstanceNode.getUncached().newInstance(cons);
        });
  }
}
