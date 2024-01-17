package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Exclusive;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.state.State;

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
  public abstract Object execute(
      Object callable,
      MaterializedFrame callerFrame,
      State state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail);

  /**
   * Only this specialization will match objects with warnings attached as they will have type
   * `WithWarnings` which is not a subtype of `Function` or other types from specializations below;
   * and the last specialization is a `Fallback`.
   */
  @Specialization(guards = "warnings.hasWarnings(warning)")
  Object invokeWithWarnings(
      Object warning,
      MaterializedFrame callerFrame,
      State state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Cached IndirectInvokeCallableNode invokeCallableNode,
      @CachedLibrary(limit = "3") WarningsLibrary warnings) {
    try {
      var result =
          invokeCallableNode.execute(
              warnings.removeWarnings(warning),
              callerFrame,
              state,
              arguments,
              schema,
              defaultsExecutionMode,
              argumentsExecutionMode,
              isTail);

      Warning[] extracted = warnings.getWarnings(warning, null, false);
      return WithWarnings.wrap(EnsoContext.get(this), result, extracted);
    } catch (UnsupportedMessageException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
    }
  }

  @Specialization
  Object invokeFunction(
      Function function,
      MaterializedFrame callerFrame,
      State state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Exclusive @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
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
  Object invokeConstructor(
      AtomConstructor constructor,
      MaterializedFrame callerFrame,
      State state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Exclusive @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
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
  Object invokeDataflowError(
      DataflowError error,
      MaterializedFrame callerFrame,
      State state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    return error;
  }

  @Specialization
  Object invokePanicSentinel(
      PanicSentinel sentinel,
      MaterializedFrame callerFrame,
      State state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    throw sentinel;
  }

  @Specialization
  public Object invokeDynamicSymbol(
      UnresolvedSymbol symbol,
      MaterializedFrame callerFrame,
      State state,
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
      Object self = arguments[thisArgumentPosition];
      if (argumentsExecutionMode.shouldExecute()) {
        self = thisExecutor.executeThunk(callerFrame, self, state, BaseNode.TailStatus.NOT_TAIL);
        arguments[thisArgumentPosition] = self;
      }
      return invokeMethodNode.execute(
          callerFrame,
          state,
          symbol,
          self,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail,
          thisArgumentPosition);
    } else {
      CompilerDirectives.transferToInterpreter();
      throw new RuntimeException("Currying without `this` argument is not yet supported.");
    }
  }

  @Fallback
  public Object invokeGeneric(
      Object callable,
      MaterializedFrame callerFrame,
      State state,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    Atom error = EnsoContext.get(this).getBuiltins().error().makeNotInvokable(callable);
    throw new PanicException(error, this);
  }
}
