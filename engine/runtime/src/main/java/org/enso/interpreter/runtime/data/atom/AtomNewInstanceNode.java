package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

/**
 * Provides unified way to construct {@link Atom} instances. Use {@link #getUncached()} to obtain a
 * singleton instance of this node and then call {@link #execute(AtomConstructor, Object[])} to
 * instiantiate an atom from its {@link AtomConstructor} and parameters passed to it. Use {@link
 * #create()} to obtain a partial evaluation ready instance to be {@link Node#insert inserted} into
 * AST hierarchy.
 */
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
   * Obtains unchached version of a node
   *
   * @return a new node with {@link #execute} method
   */
  @NeverDefault
  public static AtomNewInstanceNode getUncached() {
    return AtomNewInstanceNodeUncached.INSTANCE;
  }

  /**
   * Creates a new runtime instance of the Atom represented by this constructor.
   *
   * @param constructor the constructor to invoke
   * @param arguments the runtime arguments to the constructor
   * @return a new instance of the atom represented by this constructor
   */
  public final Atom newInstance(AtomConstructor constructor, Object... arguments) {
    return execute(constructor, arguments);
  }

  abstract Atom execute(AtomConstructor constructor, Object[] arguments);

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

  @Specialization
  @CompilerDirectives.TruffleBoundary
  Atom doSlow(AtomConstructor atom, Object[] arguments) {
    return AtomConstructorInstanceNode.uncached(this, atom, arguments);
  }
}
