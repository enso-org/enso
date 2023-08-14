package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.State;

/** A helper node to handle function application for the interop library. */
@GenerateUncached
@NodeInfo(description = "Helper node to handle function application through the interop library.")
public abstract class InteropApplicationNode extends Node {

  InteropApplicationNode() {}

  /**
   * Creates an instance of this node.
   *
   * @return an interop application node
   */
  public static InteropApplicationNode build() {
    return InteropApplicationNodeGen.create();
  }

  /**
   * Calls the function with given state and arguments.
   *
   * @param function the function to call.
   * @param state the current monadic state.
   * @param arguments the arguments for the function.
   * @return the result of calling the function.
   */
  public abstract Object execute(Function function, Object state, Object[] arguments);

  @CompilerDirectives.TruffleBoundary
  CallArgumentInfo[] buildSchema(int length) {
    CallArgumentInfo[] args = new CallArgumentInfo[length];
    for (int i = 0; i < length; i++) {
      args[i] = new CallArgumentInfo();
    }
    return args;
  }

  @CompilerDirectives.TruffleBoundary
  InvokeFunctionNode buildSorter(int length) {
    CallArgumentInfo[] args = buildSchema(length);
    return InvokeFunctionNode.build(
        args,
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  @NonIdempotent
  EnsoContext getContext() {
    return EnsoContext.get(this);
  }

  @Specialization(
      guards = {"!getContext().isInlineCachingDisabled()", "arguments.length == cachedArgsLength"},
      limit = Constants.CacheSizes.FUNCTION_INTEROP_LIBRARY)
  Object callCached(
      Function function,
      State state,
      Object[] arguments,
      @Cached("arguments.length") int cachedArgsLength,
      @Cached("buildSorter(cachedArgsLength)") InvokeFunctionNode sorterNode,
      @Shared @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode) {
    Object[] args = new Object[cachedArgsLength];
    for (int i = 0; i < cachedArgsLength; i++) {
      args[i] = hostValueToEnsoNode.execute(arguments[i]);
    }
    return sorterNode.execute(function, null, state, args);
  }

  @Specialization(replaces = "callCached")
  Object callUncached(
      Function function,
      Object state,
      Object[] arguments,
      @Cached IndirectInvokeFunctionNode indirectInvokeFunctionNode,
      @Shared @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode) {
    Object[] args = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      args[i] = hostValueToEnsoNode.execute(arguments[i]);
    }
    return indirectInvokeFunctionNode.execute(
        function,
        null,
        state,
        args,
        buildSchema(arguments.length),
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED,
        BaseNode.TailStatus.NOT_TAIL);
  }
}
