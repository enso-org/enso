package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.nodes.Node;

@ReportPolymorphism
public abstract class AtomNewInstanceNode extends Node {
  AtomNewInstanceNode() {}

  /**
   * Creates a new node to instantiate atom for provided constructor.
   *
   * @param constructor associated with the node
   * @return a new node with {@link #execute(java.lang.Object[])} method
   */
  @CompilerDirectives.TruffleBoundary
  @NeverDefault
  static AtomNewInstanceNode create(AtomConstructor constructor) {
    return AtomNewInstanceImplNode.create(constructor);
  }

  /**
   * Creates a new runtime instance of the Atom represented by this constructor.
   *
   * @param arguments the runtime arguments to the constructor
   * @return a new instance of the atom represented by this constructor
   */
  public abstract Atom execute(Object[] arguments);
}
