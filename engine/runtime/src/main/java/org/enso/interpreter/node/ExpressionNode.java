package org.enso.interpreter.node;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * A base class for all Enso expressions.
 *
 * <p>Enso is an expression-oriented language, and hence doesn't have any statements. This means
 * that all expression execution will return a value, even if that is just the {@link
 * Builtins#unit()} type.
 *
 * <p>This class contains specialisations of the {@link #executeGeneric(VirtualFrame)
 * executeGeneric} method for various scenarios in order to improve performance.
 */
@NodeInfo(shortName = "EnsoExpression", description = "The base node for all enso expressions.")
public abstract class ExpressionNode extends BaseNode {

  /**
   * Executes the current node, returning the result as a {@code long}.
   *
   * @param frame the stack frame for execution
   * @return the {@code long} value obtained by executing the node
   * @throws UnexpectedResultException if the result cannot be represented as a value of the return
   *     type
   */
  public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
    return TypesGen.expectLong(executeGeneric(frame));
  }

  /**
   * Executes the current node, returning the result as an {@link AtomConstructor}.
   *
   * @param frame the stack frame for execution
   * @return the Atom constructor obtained by executing the node
   * @throws UnexpectedResultException if the result cannot be represented as a value of the return
   *     type
   */
  public AtomConstructor executeAtomConstructor(VirtualFrame frame)
      throws UnexpectedResultException {
    return TypesGen.expectAtomConstructor(executeGeneric(frame));
  }

  /**
   * Executes the current node, returning the result as an {@link Atom}.
   *
   * @param frame the stack frame for execution
   * @return the Atom obtained by executing the node
   * @throws UnexpectedResultException if the result cannot be represented as a value of the return
   *     type
   */
  public Atom executeAtom(VirtualFrame frame) throws UnexpectedResultException {
    return TypesGen.expectAtom(executeGeneric(frame));
  }

  /**
   * Executes the current node, returning the result as a {@link Function}.
   *
   * @param frame the stack frame for execution
   * @return the function obtained by executing the node
   * @throws UnexpectedResultException if the result cannot be represented as a value of the return
   *     type
   */
  public Function executeFunction(VirtualFrame frame) throws UnexpectedResultException {
    return TypesGen.expectFunction(executeGeneric(frame));
  }

  /**
   * Executes the current node and returns a result.
   *
   * @param frame the stack frame for execution
   * @return the result of executing the node
   */
  public abstract Object executeGeneric(VirtualFrame frame);

  /**
   * Executes the current node without returning a result.
   *
   * @param frame the stack frame for execution
   */
  public void executeVoid(VirtualFrame frame) {
    executeGeneric(frame);
  }
}
