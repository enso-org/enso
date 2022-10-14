package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.tag.IdentifiedTag;

import java.util.Arrays;
import java.util.UUID;

/**
 * A node used for instrumenting function calls. It does nothing useful from the language
 * perspective, but provides the structure necessary for the instrumentation framework.
 */
@GenerateWrapper
@NodeInfo(description = "A node used for instrumenting function calls.")
public class FunctionCallInstrumentationNode extends Node implements InstrumentableNode {
  private UUID id;

  FunctionCallInstrumentationNode() {}

  /**
   * Returns an instance of this node.
   *
   * @return a new instance of this node.
   */
  public static FunctionCallInstrumentationNode build() {
    return new FunctionCallInstrumentationNode();
  }

  /**
   * Marks this node as instrumentable.
   *
   * @return true.
   */
  @Override
  public boolean isInstrumentable() {
    return true;
  }

  /** A simple value class for function call information. */
  @ExportLibrary(InteropLibrary.class)
  public static final class FunctionCall implements TruffleObject {
    private final Function function;
    private final Object state;
    private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] arguments;

    /**
     * Creates an instance of this class.
     *
     * @param function the function being called.
     * @param state the monadic state to pass to the function.
     * @param arguments the arguments passed to the function.
     */
    public FunctionCall(Function function, Object state, Object[] arguments) {
      this.function = function;
      this.state = state;
      this.arguments = arguments;
    }

    /**
     * Marks this object as executable for the purposes of the polyglot API.
     *
     * @return true
     */
    @ExportMessage
    boolean isExecutable() {
      return true;
    }

    /**
     * Handles execution through the polyglot API. Does not use the arguments provided, since the
     * function arguments are already a part of this object.
     */
    @ExportMessage
    static class Execute {
      @Specialization
      static Object callCached(
          FunctionCall functionCall,
          Object[] arguments,
          @Cached InteropApplicationNode interopApplicationNode) {
        Object[] callArguments =
            Arrays.copyOf(
                functionCall.getArguments(), functionCall.getArguments().length + arguments.length);
        System.arraycopy(
            arguments, 0, callArguments, functionCall.getArguments().length, arguments.length);
        return interopApplicationNode.execute(
            functionCall.function, functionCall.state, callArguments);
      }
    }

    /** @return the function for this call. */
    public Function getFunction() {
      return function;
    }

    /** @return the state passed to the function in this call. */
    public Object getState() {
      return state;
    }

    /** @return the arguments passed to the function in this call. */
    public Object[] getArguments() {
      return arguments;
    }
  }

  /**
   * Executes this node, by wrapping the arguments in a {@link FunctionCall} object.
   *
   * @param frame current execution frame.
   * @param function the function being called.
   * @param state the monadic state passed to the function.
   * @param arguments the arguments passed to the function.
   * @return an instance of {@link FunctionCall} containing the function, state and arguments.
   */
  public Object execute(VirtualFrame frame, Function function, Object state, Object[] arguments) {
    return new FunctionCall(function, state, arguments);
  }

  /**
   * Creates a wrapper node for the use by the instrumentation API.
   *
   * @param probeNode the probe created by the instrumentation API.
   * @return a wrapper containing both this and the probe node.
   */
  @Override
  public WrapperNode createWrapper(ProbeNode probeNode) {
    return new FunctionCallInstrumentationNodeWrapper(this, probeNode);
  }

  /**
   * Makrs this node with relevant runtime tags.
   *
   * @param tag the tag to check agains.
   * @return true if the node carries the {@code tag}, false otherwise.
   */
  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    return tag == StandardTags.CallTag.class || (tag == IdentifiedTag.class && id != null);
  }

  /** @return the source section of this node. */
  @Override
  public SourceSection getSourceSection() {
    Node parent = getParent();
    return parent == null ? null : parent.getSourceSection();
  }

  /** @return the expression ID of this node. */
  public UUID getId() {
    return id;
  }

  /**
   * Sets the expression ID of this node.
   *
   * @param expressionId the ID to assign this node.
   */
  public void setId(UUID expressionId) {
    this.id = expressionId;
  }
}
