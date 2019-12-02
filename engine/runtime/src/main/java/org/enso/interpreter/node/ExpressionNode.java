package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
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
@GenerateWrapper
public abstract class ExpressionNode extends BaseNode implements InstrumentableNode {
  private static final int NO_SOURCE = -1;
  private @CompilerDirectives.CompilationFinal int sourceStartIndex;
  private @CompilerDirectives.CompilationFinal int sourceLength;

  /** Creates a new instance of this node. */
  public ExpressionNode() {
    sourceLength = NO_SOURCE;
    sourceStartIndex = NO_SOURCE;
  }

  /**
   * Sets the source location of this node.
   *
   * @param sourceStartIndex the source index this node begins at
   * @param sourceLength the length of this node's source
   */
  public void setSourceLocation(int sourceStartIndex, int sourceLength) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.sourceStartIndex = sourceStartIndex;
    this.sourceLength = sourceLength;
  }

  /**
   * Creates a source section this node represents.
   *
   * @return a source section for this node
   */
  @Override
  public SourceSection getSourceSection() {
    if (sourceStartIndex == NO_SOURCE) {
      return null;
    }
    RootNode rootNode = getRootNode();
    if (rootNode == null) {
      return null;
    }
    SourceSection rootSourceSection = rootNode.getSourceSection();
    if (rootSourceSection == null) {
      return null;
    }
    return rootSourceSection.getSource().createSection(sourceStartIndex, sourceLength);
  }

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

  /**
   * Marks this node as instrumentable by Truffle Instrumentation APIs.
   *
   * @return {@code true}
   */
  @Override
  public boolean isInstrumentable() {
    return true;
  }

  /**
   * Wraps this node with an instrumentation probe. For internal use by the Truffle framework.
   *
   * @param probe the probe to attach to this node
   * @return a wrapper delegating both to this node and the probe
   */
  @Override
  public WrapperNode createWrapper(ProbeNode probe) {
    return new ExpressionNodeWrapper(this, probe);
  }
}
