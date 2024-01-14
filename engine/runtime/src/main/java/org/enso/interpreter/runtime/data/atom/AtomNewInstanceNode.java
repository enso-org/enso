package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

@ReportPolymorphism
public abstract class AtomNewInstanceNode extends Node {
  AtomNewInstanceNode() {}

  /**
   * Creates a new node to instantiate atom for provided constructor.
   *
   * @return a new node with {@link #execute} method
   */
  @CompilerDirectives.TruffleBoundary
  @NeverDefault
  public static AtomNewInstanceNode create() {
    return AtomNewInstanceNodeGen.create();
  }

  /**
   * Creates a new runtime instance of the Atom represented by this constructor.
   *
   * @param constructor the constructor to invoke
   * @param arguments the runtime arguments to the constructor
   * @return a new instance of the atom represented by this constructor
   */
  public abstract Atom execute(AtomConstructor constructor, Object[] arguments);

  @Specialization(
      limit = "3",
      guards = {"c == cachedC"})
  final Atom instanceWithConstructor(
      AtomConstructor c,
      Object[] arguments,
      @Cached("c") AtomConstructor cachedC,
      @Cached("createConstructorNode(cachedC)") AtomConstructorInstanceNode node) {
    return node.execute(arguments);
  }

  static AtomConstructorInstanceNode createConstructorNode(AtomConstructor c) {
    return AtomConstructorInstanceNode.create(c);
  }
}
