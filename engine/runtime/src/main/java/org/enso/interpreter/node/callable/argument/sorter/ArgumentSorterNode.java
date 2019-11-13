package org.enso.interpreter.node.callable.argument.sorter;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * This class represents the protocol for remapping the arguments provided at a call site into the
 * positional order expected by the definition of the {@link Function}.
 */
@NodeInfo(shortName = "ArgumentSorter")
public abstract class ArgumentSorterNode extends BaseNode {

  private @CompilationFinal(dimensions = 1) CallArgumentInfo[] schema;
  private final InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode;
  private final InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode;

  /**
   * Creates a node that performs the argument organisation for the provided schema.
   *
   * @param schema information about the call arguments in positional order
   * @param defaultsExecutionMode the defaults execution mode for this function invocation
   * @param argumentsExecutionMode the arguments execution mode for this function invocation
   */
  public ArgumentSorterNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    this.schema = schema;
    this.defaultsExecutionMode = defaultsExecutionMode;
    this.argumentsExecutionMode = argumentsExecutionMode;
  }

  /**
   * Generates the argument mapping where it has already been computed and executes the function.
   *
   * <p>This specialisation is executed in the cases where the interpreter has already computed the
   * mapping necessary to reorder call-stite arguments into the order expected by the definition
   * site. It is also a fast path.
   *
   * <p>This specialisation can only execute when the {@link Function} provided to the method
   * matches with the one stored in the cached argument sorter object.
   *
   * @param function the function to sort arguments for
   * @param state the state to pass to the function
   * @param arguments the arguments being passed to {@code callable}
   * @param mappingNode a cached node that tracks information about the mapping to enable a fast
   *     path
   * @param optimiser a cached call optimizer node, capable of performing the actual function call
   * @return the result of applying the function with remapped arguments
   */
  @Specialization(
      guards = "mappingNode.isCompatible(function)",
      limit = Constants.CacheSizes.ARGUMENT_SORTER_NODE)
  public Stateful invokeCached(
      Function function,
      Object state,
      Object[] arguments,
      @Cached(
              "build(function, getSchema(), getDefaultsExecutionMode(), getArgumentsExecutionMode(), isTail())")
          CachedArgumentSorterNode mappingNode) {
    return mappingNode.execute(function, state, arguments);
  }

  /**
   * Generates an argument mapping and executes a function with properly ordered arguments. Does not
   * perform any caching and is thus a slow-path operation.
   *
   * @param function the function to execute.
   * @param state the state to pass to the function
   * @param arguments the arguments to reorder and supply to the {@code function}.
   * @return the result of calling {@code function} with the supplied {@code arguments}.
   */
  @Specialization(replaces = "invokeCached")
  public Stateful invokeUncached(Function function, Object state, Object[] arguments) {
    return invokeCached(
        function,
        state,
        arguments,
        CachedArgumentSorterNode.build(
            function,
            getSchema(),
            getDefaultsExecutionMode(),
            getArgumentsExecutionMode(),
            isTail()));
  }

  /**
   * Executes the {@link ArgumentSorterNode} to reorder the arguments.
   *
   * @param callable the function to sort arguments for
   * @param state the state to pass to the function
   * @param arguments the arguments being passed to {@code function}
   * @return the result of executing the {@code function} with reordered {@code arguments}
   */
  public abstract Stateful execute(Function callable, Object state, Object[] arguments);

  CallArgumentInfo[] getSchema() {
    return schema;
  }

  InvokeCallableNode.DefaultsExecutionMode getDefaultsExecutionMode() {
    return this.defaultsExecutionMode;
  }

  InvokeCallableNode.ArgumentsExecutionMode getArgumentsExecutionMode() {
    return argumentsExecutionMode;
  }
}
