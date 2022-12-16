package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo.ArgumentMapping;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.state.State;

import java.util.concurrent.locks.Lock;

/**
 * This class handles the case where a mapping for reordering arguments to a given callable has
 * already been computed.
 */
@NodeInfo(description = "Reorders the arguments and executes them, according to a provided mapping")
public class ArgumentSorterNode extends BaseNode {
  private final FunctionSchema preApplicationSchema;
  private final FunctionSchema postApplicationSchema;
  private final ArgumentMapping mapping;
  private @Children ThunkExecutorNode[] executors;
  private final InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode;

  private ArgumentSorterNode(
      FunctionSchema preApplicationSchema,
      FunctionSchema postApplicationSchema,
      ArgumentMapping mapping,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    this.preApplicationSchema = preApplicationSchema;
    this.argumentsExecutionMode = argumentsExecutionMode;
    this.mapping = mapping;
    this.postApplicationSchema = postApplicationSchema;
  }

  /**
   * Creates an instance of this node.
   *
   * @param preApplicationSchema the schema of all functions passed to the {@link #execute(Function,
   *     Object, Object[])} method of this node.
   * @param mapping the argument mapping generated for current application site.
   * @param argumentsExecutionMode lazy arguments handling mode for this node.
   * @return a sorter node for the arguments in {@code schema} being passed to a function with the
   *     {@code preApplicationSchema}.
   */
  public static ArgumentSorterNode build(
      FunctionSchema preApplicationSchema,
      ArgumentMapping mapping,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    return new ArgumentSorterNode(
        preApplicationSchema, mapping.getPostApplicationSchema(), mapping, argumentsExecutionMode);
  }

  private void initArgumentExecutors() {
    ThunkExecutorNode[] executors =
        new ThunkExecutorNode[mapping.getArgumentShouldExecute().length];
    for (int i = 0; i < mapping.getArgumentShouldExecute().length; i++) {
      if (mapping.getArgumentShouldExecute()[i]) {
        executors[i] = insert(ThunkExecutorNode.build());
      }
    }
    this.executors = executors;
  }

  @ExplodeLoop
  private void executeArguments(Object[] arguments, State state) {
    if (executors == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      Lock lock = getLock();
      lock.lock();
      try {
        if (executors == null) {
          initArgumentExecutors();
        }
      } finally {
        lock.unlock();
      }
    }
    for (int i = 0; i < mapping.getArgumentShouldExecute().length; i++) {
      if (executors[i] != null) {
        arguments[i] = executors[i].executeThunk(arguments[i], state, TailStatus.NOT_TAIL);
      }
    }
  }

  /**
   * Reorders the provided arguments into the necessary order for the cached callable.
   *
   * @param function the function this node is reordering arguments for
   * @param state the state to pass to the function
   * @param arguments the arguments to reorder
   * @return the provided {@code arguments} in the order expected by the cached {@link Function}
   */
  public MappedArguments execute(Function function, State state, Object[] arguments) {
    if (argumentsExecutionMode.shouldExecute()) {
      executeArguments(arguments, state);
    }
    Object[] mappedAppliedArguments =
        prepareArguments(
            this.preApplicationSchema,
            this.postApplicationSchema,
            this.mapping,
            function,
            arguments);
    Object[] oversaturatedArguments = null;
    if (postApplicationSchema.hasOversaturatedArgs()) {
      oversaturatedArguments = generateOversaturatedArguments(function, arguments);
    }
    return new MappedArguments(mappedAppliedArguments, oversaturatedArguments);
  }

  @ExplodeLoop
  static Object[] prepareArguments(
      FunctionSchema preSchema,
      FunctionSchema postSchema,
      ArgumentMapping map,
      Function fn,
      Object[] args) {
    Object[] mappedAppliedArguments;
    if (preSchema.hasAnyPreApplied()) {
      mappedAppliedArguments = new Object[preSchema.getArgumentsCount()];
      for (int i = 0; i < mappedAppliedArguments.length; i++) {
        mappedAppliedArguments[i] = fn.getPreAppliedArguments()[i];
      }
    } else {
      mappedAppliedArguments = new Object[postSchema.getArgumentsCount()];
    }
    map.reorderAppliedArguments(args, mappedAppliedArguments);
    return mappedAppliedArguments;
  }

  /**
   * Generates an array containing the oversaturated arguments for the function being executed (if
   * any).
   *
   * <p>It accounts for oversaturated arguments at the function call site, as well as any that have
   * been 'remembered' in the passed {@link Function} object.
   *
   * @param function the function being executed
   * @param arguments the arguments being applied to {@code function}
   * @return any oversaturated arguments on {@code function}
   */
  private Object[] generateOversaturatedArguments(Function function, Object[] arguments) {
    Object[] oversaturatedArguments =
        new Object[this.postApplicationSchema.getOversaturatedArguments().length];

    System.arraycopy(
        function.getOversaturatedArguments(),
        0,
        oversaturatedArguments,
        0,
        preApplicationSchema.getOversaturatedArguments().length);

    mapping.obtainOversaturatedArguments(
        arguments, oversaturatedArguments, preApplicationSchema.getOversaturatedArguments().length);

    return oversaturatedArguments;
  }

  public static class MappedArguments {
    private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] sortedArguments;
    private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[]
        oversaturatedArguments;

    public MappedArguments(Object[] sortedArguments, Object[] oversaturatedArguments) {
      this.sortedArguments = sortedArguments;
      this.oversaturatedArguments = oversaturatedArguments;
    }

    /**
     * Gets the reordered and pre-executed arguments, ready to pass to a function.
     *
     * @return the sorted and executed arguments.
     */
    public Object[] getSortedArguments() {
      return sortedArguments;
    }

    /**
     * Gets the left over (not expected by the current function) arguments.
     *
     * @return a collection of left over arguments.
     */
    public Object[] getOversaturatedArguments() {
      return oversaturatedArguments;
    }
  }
}
