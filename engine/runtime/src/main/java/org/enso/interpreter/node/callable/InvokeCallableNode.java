package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.argument.ThunkExecutorNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNodeGen;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.NotInvokableException;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * This class is responsible for performing the actual invocation of a given callable with its
 * arguments.
 *
 * <p>This invocation is segmented via the type of the callable, allowing handling of the various
 * different kinds within a seamless framework.
 */
public abstract class InvokeCallableNode extends BaseNode {

  /** Denotes the mode of defaulted arguments treatment for a function invocation. */
  public enum DefaultsExecutionMode {
    /** Defaulted arguments should be ignored for this application position. */
    IGNORE,
    /** Defaulted arguments should be executed normally for this application position. */
    EXECUTE;

    /**
     * Should the presence of defaulted arguments be ignored for this application position?
     *
     * @return {@code true} if the defaulted arguments should be ignored, {@code false} otherwise
     */
    public boolean isIgnore() {
      return this == IGNORE;
    }
  }

  /** Denotes the mode of arguments execution for a function invocation */
  public enum ArgumentsExecutionMode {
    /** Arguments are pre-executed for this call. */
    PRE_EXECUTED,
    /**
     * Arguments are passed as {@link Thunk}s and should be executed before calling the function.
     */
    EXECUTE;

    /**
     * Should the arguments be executed before calling the function?
     *
     * @return {@code true} if the arguments should be executed, {@code false} otherwise
     */
    public boolean shouldExecute() {
      return this == EXECUTE;
    }
  }

  @Child private ArgumentSorterNode argumentSorter;
  @Child private MethodResolverNode methodResolverNode;
  @Child private ThunkExecutorNode thisExecutor;

  private final boolean canApplyThis;
  private final int thisArgumentPosition;

  private final ArgumentsExecutionMode argumentsExecutionMode;

  InvokeCallableNode(
      CallArgumentInfo[] schema,
      DefaultsExecutionMode defaultsExecutionMode,
      ArgumentsExecutionMode argumentsExecutionMode) {
    boolean appliesThis = false;
    int idx = 0;
    for (; idx < schema.length; idx++) {
      CallArgumentInfo arg = schema[idx];

      boolean isNamedThis = arg.isNamed() && arg.getName().equals(Constants.THIS_ARGUMENT_NAME);
      if (arg.isPositional() || isNamedThis) {
        appliesThis = true;
        break;
      }
    }

    this.canApplyThis = appliesThis;
    this.thisArgumentPosition = idx;

    this.argumentsExecutionMode = argumentsExecutionMode;

    this.argumentSorter =
        ArgumentSorterNodeGen.create(schema, defaultsExecutionMode, argumentsExecutionMode);
    this.methodResolverNode = MethodResolverNodeGen.create();
  }

  /**
   * Creates a new instance of this node.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param defaultsExecutionMode the defaulted arguments handling mode for this call
   * @param argumentsExecutionMode the arguments execution mode for this call
   */
  public static InvokeCallableNode build(
      CallArgumentInfo[] schema,
      DefaultsExecutionMode defaultsExecutionMode,
      ArgumentsExecutionMode argumentsExecutionMode) {
    return InvokeCallableNodeGen.create(schema, defaultsExecutionMode, argumentsExecutionMode);
  }

  /**
   * Invokes a function directly on the arguments contained in this node.
   *
   * @param function the function to be executed
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to the function
   * @return the result of executing {@code callable} on the known arguments
   */
  @Specialization
  Stateful invokeFunction(
      Function function, VirtualFrame callerFrame, Object state, Object[] arguments) {
    return this.argumentSorter.execute(function, callerFrame, state, arguments);
  }

  /**
   * Invokes a constructor directly on the arguments contained in this node.
   *
   * @param constructor the constructor to be executed
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to the constructor
   * @return the result of executing {@code constructor} on the known arguments
   */
  @Specialization
  Stateful invokeConstructor(
      AtomConstructor constructor, VirtualFrame callerFrame, Object state, Object[] arguments) {
    return invokeFunction(constructor.getConstructorFunction(), callerFrame, state, arguments);
  }

  /**
   * Invokes a dynamic symbol after resolving it for the actual symbol for the {@code this}
   * argument.
   *
   * @param symbol the name of the requested symbol
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to the dynamic symbol
   * @return the result of resolving and executing the symbol for the {@code this} argument
   */
  @Specialization
  public Stateful invokeDynamicSymbol(
      UnresolvedSymbol symbol, VirtualFrame callerFrame, Object state, Object[] arguments) {
    if (canApplyThis) {
      Object selfArgument = arguments[thisArgumentPosition];
      if (argumentsExecutionMode.shouldExecute()) {
        if (thisExecutor == null) {
          CompilerDirectives.transferToInterpreterAndInvalidate();
          thisExecutor = insert(ThunkExecutorNode.build(false));
        }
        Stateful selfResult = thisExecutor.executeThunk((Thunk) selfArgument, state);
        selfArgument = selfResult.getValue();
        state = selfResult.getState();
      }
      Function function = methodResolverNode.execute(symbol, selfArgument);
      return this.argumentSorter.execute(function, callerFrame, state, arguments);
    } else {
      throw new RuntimeException("Currying without `this` argument is not yet supported.");
    }
  }

  /**
   * A fallback that should never be called.
   *
   * <p>If this is called, something has gone horribly wrong. It throws a {@link
   * NotInvokableException} to signal this.
   *
   * @param callable the callable to be executed
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to the callable
   * @return error
   */
  @Fallback
  public Stateful invokeGeneric(
      Object callable, VirtualFrame callerFrame, Object state, Object[] arguments) {
    throw new NotInvokableException(callable, this);
  }

  /**
   * Executes the provided {@code callable} on the supplied {@code arguments}.
   *
   * @param callable the callable to evaluate
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to evaluate {@code callable} on
   * @return the result of executing {@code callable} on the supplied {@code arguments}
   */
  public abstract Stateful execute(
      Object callable, VirtualFrame callerFrame, Object state, Object[] arguments);

  /**
   * Sets whether or not the current node is tail-recursive.
   *
   * @param isTail whether or not the node is tail-recursive.
   */
  @Override
  public void setTail(boolean isTail) {
    super.setTail(isTail);
    argumentSorter.setTail(isTail);
  }
}
