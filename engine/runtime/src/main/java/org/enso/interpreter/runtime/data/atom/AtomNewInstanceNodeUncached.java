package org.enso.interpreter.runtime.data.atom;

class AtomNewInstanceNodeUncached extends AtomNewInstanceNode {
  static final AtomNewInstanceNode INSTANCE = new AtomNewInstanceNodeUncached();

  AtomNewInstanceNodeUncached() {}

  @Override
  public Atom execute(AtomConstructor constructor, Object[] arguments) {
    return constructor.newInstance(arguments);
  }
}
