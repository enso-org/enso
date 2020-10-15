package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.CaptureCallerInfoNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.argument.ArgumentSorterNode;
import org.enso.interpreter.node.callable.argument.IndirectArgumentSorterNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.UUID;

/**
 * This class represents the protocol for remapping the arguments provided at a call site into the
 * positional order expected by the definition of the {@link Function}.
 */
@NodeInfo(shortName = "ArgumentSorter")
@ImportStatic({CallArgumentInfo.ArgumentMappingBuilder.class, Constants.CacheSizes.class})
public abstract class InvokeFunctionNode extends BaseNode {

  private @CompilationFinal(dimensions = 1) CallArgumentInfo[] schema;
  private final InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode;
  private final InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode;
  private @Child CaptureCallerInfoNode captureCallerInfoNode = CaptureCallerInfoNode.build();
  private @Child FunctionCallInstrumentationNode functionCallInstrumentationNode =
      FunctionCallInstrumentationNode.build();

  InvokeFunctionNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    this.schema = schema;
    this.defaultsExecutionMode = defaultsExecutionMode;
    this.argumentsExecutionMode = argumentsExecutionMode;
  }

  /**
   * Creates an instance of this node.
   *
   * @param schema information about the call arguments in positional order
   * @param defaultsExecutionMode the defaults execution mode for this function invocation
   * @param argumentsExecutionMode the arguments execution mode for this function invocation
   * @return an instance of this node.
   */
  public static InvokeFunctionNode build(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    return InvokeFunctionNodeGen.create(schema, defaultsExecutionMode, argumentsExecutionMode);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "function.getSchema() == cachedSchema"},
      limit = Constants.CacheSizes.ARGUMENT_SORTER_NODE)
  Stateful invokeCached(
      Function function,
      VirtualFrame callerFrame,
      Object state,
      Object[] arguments,
      @CachedContext(Language.class) Context context,
      @Cached("function.getSchema()") FunctionSchema cachedSchema,
      @Cached("generate(cachedSchema, getSchema())")
          CallArgumentInfo.ArgumentMapping argumentMapping,
      @Cached("build(cachedSchema, argumentMapping, getArgumentsExecutionMode())")
          ArgumentSorterNode mappingNode,
      @Cached(
              "build(argumentMapping, getDefaultsExecutionMode(), getArgumentsExecutionMode(), getTailStatus())")
          CurryNode curryNode) {
    ArgumentSorterNode.MappedArguments mappedArguments =
        mappingNode.execute(function, state, arguments);
    CallerInfo callerInfo = null;
    if (cachedSchema.getCallerFrameAccess().shouldFrameBePassed()) {
      callerInfo = captureCallerInfoNode.execute(callerFrame.materialize());
    }
    functionCallInstrumentationNode.execute(
        callerFrame, function, mappedArguments.getState(), mappedArguments.getSortedArguments());
    return curryNode.execute(
        callerFrame,
        function,
        callerInfo,
        mappedArguments.getState(),
        mappedArguments.getSortedArguments(),
        mappedArguments.getOversaturatedArguments());
  }

  /**
   * Generates an argument mapping and executes a function with properly ordered arguments. Does not
   * perform any caching and is thus a slow-path operation.
   *
   * @param function the function to execute.
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to reorder and supply to the {@code function}.
   * @return the result of calling {@code function} with the supplied {@code arguments}.
   */
  @Specialization(replaces = "invokeCached")
  Stateful invokeUncached(
      Function function,
      VirtualFrame callerFrame,
      Object state,
      Object[] arguments,
      @Cached IndirectArgumentSorterNode mappingNode,
      @Cached IndirectCurryNode curryNode) {
    CallArgumentInfo.ArgumentMapping argumentMapping =
        CallArgumentInfo.ArgumentMappingBuilder.generate(function.getSchema(), getSchema());

    ArgumentSorterNode.MappedArguments mappedArguments =
        mappingNode.execute(
            function.getSchema(),
            argumentMapping,
            getArgumentsExecutionMode(),
            function,
            state,
            arguments);

    CallerInfo callerInfo = null;

    if (function.getSchema().getCallerFrameAccess().shouldFrameBePassed()) {
      callerInfo = captureCallerInfoNode.execute(callerFrame.materialize());
    }

    functionCallInstrumentationNode.execute(
        callerFrame, function, mappedArguments.getState(), mappedArguments.getSortedArguments());

    return curryNode.execute(
        callerFrame == null ? null : callerFrame.materialize(),
        function,
        callerInfo,
        mappedArguments.getState(),
        mappedArguments.getSortedArguments(),
        mappedArguments.getOversaturatedArguments(),
        argumentMapping.getPostApplicationSchema(),
        defaultsExecutionMode,
        argumentsExecutionMode,
        getTailStatus());
  }

  /**
   * Executes the {@link InvokeFunctionNode} to apply the function to given arguments.
   *
   * @param callable the function to call
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments being passed to {@code function}
   * @return the result of executing the {@code function} with reordered {@code arguments}
   */
  public abstract Stateful execute(
      Function callable, VirtualFrame callerFrame, Object state, Object[] arguments);

  CallArgumentInfo[] getSchema() {
    return schema;
  }

  InvokeCallableNode.DefaultsExecutionMode getDefaultsExecutionMode() {
    return this.defaultsExecutionMode;
  }

  InvokeCallableNode.ArgumentsExecutionMode getArgumentsExecutionMode() {
    return argumentsExecutionMode;
  }

  /** @return the source section for this node. */
  @Override
  public SourceSection getSourceSection() {
    Node parent = getParent();
    return parent == null ? null : parent.getSourceSection();
  }

  /**
   * Sets the expression ID of this node.
   *
   * @param id the expression ID to assign this node.
   */
  public void setId(UUID id) {
    functionCallInstrumentationNode.setId(id);
  }
}
