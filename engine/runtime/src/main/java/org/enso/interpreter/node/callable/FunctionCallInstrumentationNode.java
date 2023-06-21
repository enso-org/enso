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
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.tag.IdentifiedTag;

import java.util.Arrays;
import java.util.UUID;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;

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
    private final ModuleScope ctx;
    private final Function function;
    private final State state;
    private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] arguments;

    /**
     * Creates an instance of this class.
     *
     * @param ctx the scope where the instrumentation happens
     * @param function the function being called.
     * @param state the monadic state to pass to the function.
     * @param arguments the arguments passed to the function.
     */
    public FunctionCall(ModuleScope ctx, Function function, State state, Object[] arguments) {
      this.ctx = ctx;
      this.function = function;
      if ("Widgets.get_widget_json".equals(function.getName())) {
        Thread.dumpStack();
      }
      this.state = state;
      this.arguments = arguments;
    }

    /**
     * Creates an instance of this class.
     *
     * @param ctx the node where the instrumentation happens
     * @param function the function being called.
     * @param state the monadic state to pass to the function.
     * @param arguments the arguments passed to the function.
     */
    public FunctionCall(Node ctx, Function function, State state, Object[] arguments) {
      this(findModuleScope(ctx), function, state, arguments);
    }

    public FunctionCall(Node ctx, FunctionCall self) {
      this(ctx, self.function, self.state, self.arguments);
    }

    private static ModuleScope findModuleScope(Node ctx) {
      return switch (ctx.getRootNode()) {
        case EnsoRootNode root -> root.getModuleScope();
        case null -> null;
        default -> null;
      };
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
     * Handles execution through the polyglot API. Merges provide arguments with the ones
     * that are already a part of this object.
     */
    @ExportMessage
    static class Execute {
      @Specialization
      static Object callCached(
        FunctionCall functionCall,
        Object[] arguments,
        @Cached InteropApplicationNode interopApplicationNode,
        @CachedLibrary(limit = "10") DynamicObjectLibrary objects
      ) {
        var args = Arrays.copyOf(
          functionCall.getArguments(), functionCall.getArguments().length + arguments.length
        );
        System.arraycopy(arguments, 0, args, functionCall.getArguments().length, arguments.length);

        var state = functionCall.state;
        var old = objects.getOrDefault(state.getContainer(), 43L, null);
        try {
          objects.put(state.getContainer(), 43L, functionCall.ctx);
          return interopApplicationNode.execute(functionCall.function, state, args);
        } finally {
          objects.put(state.getContainer(), 43L, old);
        }
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
  public Object execute(VirtualFrame frame, Function function, State state, Object[] arguments) {
    return new FunctionCall(this, function, state, arguments);
  }

  /**
   * Creates a wrapper node for the use by the instrumentation API.
   *
   * @param probeNode the probe created by the instrumentation API.
   * @return a wrapper containing both this and the probe node.
   */
  @Override
  public WrapperNode createWrapper(ProbeNode probeNode) {
    var wrapper = new FunctionCallInstrumentationNodeWrapper(this, probeNode);
    wrapper.setId(this.getId());
    return wrapper;
  }

  /**
   * Marks this node with relevant runtime tags.
   *
   * @param tag the tag to check against.
   * @return true if the node carries the {@code tag}, false otherwise.
   */
  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    if (AvoidIdInstrumentationTag.class == tag) {
      return getRootNode() instanceof ClosureRootNode c && !c.isSubjectToInstrumentation();
    }
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
