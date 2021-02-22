package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * Invokes any callable with given arguments.
 *
 * <p>This is a slow-path node for the uncached flow.
 */
@GenerateUncached
public abstract class IndirectInvokeCallableNode extends Node {

  /**
   * Executes the callable with given arguments.
   *
   * @param callable the callable to call.
   * @param callerFrame current stack frame.
   * @param state current monadic state.
   * @param arguments arguments to pass to the callable.
   * @param schema names and ordering of the arguments.
   * @param defaultsExecutionMode whether defaults are suspended for this call.
   * @param argumentsExecutionMode whether arguments are preexecuted for this call.
   * @param isTail is the call happening in a tail position.
   * @return the result of executing the callable.
   */
  public abstract Stateful execute(
      Object callable,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail);

  @Specialization
  Stateful invokeFunction(
      Function function,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    return invokeFunctionNode.execute(
        function,
        callerFrame,
        state,
        arguments,
        schema,
        defaultsExecutionMode,
        argumentsExecutionMode,
        isTail);
  }

  @Specialization
  Stateful invokeConstructor(
      AtomConstructor constructor,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    return invokeFunction(
        constructor.getConstructorFunction(),
        callerFrame,
        state,
        arguments,
        schema,
        defaultsExecutionMode,
        argumentsExecutionMode,
        isTail,
        invokeFunctionNode);
  }

  @Specialization
  Stateful invokeDataflowError(
      DataflowError error,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    return new Stateful(state, error);
  }

  @Specialization
  Stateful invokePanicSentinel(
      PanicSentinel sentinel,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    throw sentinel;
  }

  @Specialization
  public Stateful invokeDynamicSymbol(
      UnresolvedSymbol symbol,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Cached IndirectInvokeMethodNode invokeMethodNode,
      @Cached ThunkExecutorNode thisExecutor) {
    Integer thisArg = InvokeCallableNode.thisArgumentPosition(schema);
    boolean canApplyThis = thisArg != null;
    int thisArgumentPosition = thisArg == null ? 0 : thisArg;
    if (canApplyThis) {
      Object selfArgument = arguments[thisArgumentPosition];
      if (argumentsExecutionMode.shouldExecute()) {
        Stateful selfResult =
            thisExecutor.executeThunk(selfArgument, state, BaseNode.TailStatus.NOT_TAIL);
        selfArgument = selfResult.getValue();
        state = selfResult.getState();
        arguments[thisArgumentPosition] = selfArgument;
      }
      return invokeMethodNode.execute(
          callerFrame,
          state,
          symbol,
          selfArgument,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    } else {
      throw new RuntimeException("Currying without `this` argument is not yet supported.");
    }
  }

  @Fallback
  public Stateful invokeGeneric(
      Object callable,
      MaterializedFrame callerFrame,
      Object state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    Context ctx = lookupContextReference(Language.class).get();
    Atom error = ctx.getBuiltins().error().makeNotInvokableError(callable);
    throw new PanicException(error, this);
  }
}
