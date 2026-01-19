package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;

public final class Ordering {
  private final Type type;

  Ordering(Type type) {
    this.type = type;
  }

  public Type getType() {
    return type;
  }

  @CompilerDirectives.TruffleBoundary
  public Atom newEqual() {
    return toAtom(type.getConstructors().get("Equal"));
  }

  @CompilerDirectives.TruffleBoundary
  public Atom newLess() {
    return toAtom(type.getConstructors().get("Less"));
  }

  @CompilerDirectives.TruffleBoundary
  public Atom newGreater() {
    return toAtom(type.getConstructors().get("Greater"));
  }

  private Atom toAtom(AtomConstructor c) {
    return AtomNewInstanceNode.getUncached().newInstance(c);
  }
}
