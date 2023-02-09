package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.GenerateWrapper.OutgoingConverter;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.NodeLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;

import java.util.UUID;

import org.enso.interpreter.instrument.HostObjectDebugWrapper;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.DebugLocalScope;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.runtime.tag.Patchable;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * A base class for all Enso expressions.
 *
 * <p>Enso is an expression-oriented language, and hence doesn't have any statements. This means
 * that all expression execution will return a value, even if that is just the {@link
 * Builtins#nothing()} type.
 *
 * <p>This class contains specialisations of the {@link #executeGeneric(VirtualFrame)
 * executeGeneric} method for various scenarios in order to improve performance.
 */
@NodeInfo(shortName = "EnsoExpression", description = "The base node for all enso expressions.")
@ExportLibrary(NodeLibrary.class)
@GenerateWrapper
public abstract class ExpressionNode extends BaseNode implements InstrumentableNode {
  private @CompilationFinal int sourceStartIndex;
  private @CompilationFinal int sourceLength;
  private @CompilationFinal UUID id = null;

  public static boolean isWrapper(ExpressionNode node) {
    return node instanceof ExpressionNodeWrapper;
  }

  /**
   * Creates a new instance of this node.
   */
  public ExpressionNode() {
    sourceLength = EnsoRootNode.NO_SOURCE;
    sourceStartIndex = EnsoRootNode.NO_SOURCE;
  }

  /**
   * Sets the source location of this node.
   *
   * @param sourceStartIndex the source index this node begins at
   * @param sourceLength     the length of this node's source
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
    if (this instanceof ExpressionNodeWrapper wrapper) {
      return wrapper.getDelegateNode().getSourceSection();
    } else {
      return EnsoRootNode.findSourceSection(getRootNode(), sourceStartIndex, sourceLength);
    }
  }

  /**
   * Gets the ID associated with this node.
   *
   * @return this node's ID.
   */
  public UUID getId() {
    return id;
  }

  /**
   * Sets the value for this node's ID.
   *
   * @param id the ID for this node.
   */
  public void setId(UUID id) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.id = id;
  }

  /**
   * Executes the current node, returning the result as a {@code long}.
   *
   * @param frame the stack frame for execution
   * @return the {@code long} value obtained by executing the node
   * @throws UnexpectedResultException if the result cannot be represented as a value of the return
   *                                   type
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
   *                                   type
   */
  public AtomConstructor executeAtomConstructor(VirtualFrame frame)
      throws UnexpectedResultException {
    return TypesGen.expectAtomConstructor(executeGeneric(frame));
  }

  /**
   * Executes the current node, returning the result as a {@link Function}.
   *
   * @param frame the stack frame for execution
   * @return the function obtained by executing the node
   * @throws UnexpectedResultException if the result cannot be represented as a value of the return
   *                                   type
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
   * Marks this node with relevant instrumentation tags.
   *
   * @param tag the tag to check against.
   * @return true if the node is tagged with the given tag, false otherwise.
   */
  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    if (tag == Patchable.Tag.class && this instanceof Patchable) {
      return true;
    }
    if (AvoidIdInstrumentationTag.class == tag) {
      return getRootNode() instanceof ClosureRootNode c && !c.isSubjectToInstrumentation();
    }
    if (tag == StandardTags.ExpressionTag.class) {
      return getSourceSection() != null;
    }
    return tag == IdentifiedTag.class && id != null;
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

  /**
   * Transitively converts the given value to a wrapper that treats the host objects
   * as simple strings. This is a workaround for https://github.com/oracle/graal/issues/5513
   * - there is a bug in chromeinspector which reinterprets host objects in host original
   * language, which causes NullPointerException. Therefore, we have to wrap all the
   * host objects.
   *
   * @param retValue Value returned from this expression node
   * @return Value with all the host objects wrapped.
   */
  @OutgoingConverter
  public Object wrapHostObjects(Object retValue) {
    // Wrap only if chrome inspector is attached.
    if (EnsoContext.get(this).getChromeInspectorNotAttached().isValid()) {
      return retValue;
    } else {
      return HostObjectDebugWrapper.wrapHostValues(retValue, InteropLibrary.getUncached(), StructsLibrary.getUncached());
    }
  }

  @ExportMessage
  boolean hasScope(Frame frame) {
    return isInstrumentable();
  }

  @ExportMessage
  Object getScope(Frame frame, boolean onEnter) {
    RootNode rootNode = getRootNode();
    if (!isInstrumentable() || rootNode == null) {
      return null;
    } else {
      return DebugLocalScope.createFromFrame((EnsoRootNode) rootNode, frame.materialize());
    }
  }
}
