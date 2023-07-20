package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NonIdempotent;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.state.State;

/** A helper node to handle method application for the interop library. */
@GenerateUncached
@NodeInfo(description = "Helper node to handle method application through the interop library.")
public abstract class InteropMethodCallNode extends Node {

  /**
   * Creates an instance of this node.
   *
   * @return an interop application node
   */
  public static InteropMethodCallNode build() {
    return InteropMethodCallNodeGen.create();
  }

  /**
   * Calls the method with given state and arguments.
   *
   * @param method the method to call.
   * @param state the current monadic state.
   * @param arguments the arguments for the function.
   * @return the result of calling the function.
   */
  public abstract Object execute(UnresolvedSymbol method, Object state, Object[] arguments)
      throws ArityException;

  @CompilerDirectives.TruffleBoundary
  CallArgumentInfo[] buildSchema(int length) {
    CallArgumentInfo[] args = new CallArgumentInfo[length];
    for (int i = 0; i < length; i++) {
      args[i] = new CallArgumentInfo();
    }
    return args;
  }

  @CompilerDirectives.TruffleBoundary
  InvokeMethodNode buildSorter(int length) {
    CallArgumentInfo[] args = buildSchema(length);
    return InvokeMethodNode.build(
        args,
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED,
        0);
  }

  @NonIdempotent
  EnsoContext getContext() {
    return EnsoContext.get(this);
  }

  @Specialization(
      guards = {"!getContext().isInlineCachingDisabled()", "arguments.length == cachedArgsLength"},
      limit = Constants.CacheSizes.FUNCTION_INTEROP_LIBRARY)
  Object callCached(
      UnresolvedSymbol method,
      State state,
      Object[] arguments,
      @Cached("arguments.length") int cachedArgsLength,
      @Cached("buildSorter(cachedArgsLength)") InvokeMethodNode sorterNode,
      @Shared @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode)
      throws ArityException {
    Object[] args = new Object[cachedArgsLength];
    for (int i = 0; i < cachedArgsLength; i++) {
      args[i] = hostValueToEnsoNode.execute(arguments[i]);
    }
    if (arguments.length == 0) throw ArityException.create(1, -1, 0);
    return sorterNode.execute(null, state, method, args[0], args);
  }

  @Specialization(replaces = "callCached")
  Object callUncached(
      UnresolvedSymbol method,
      State state,
      Object[] arguments,
      @Cached IndirectInvokeMethodNode indirectInvokeMethodNode,
      @Shared @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode)
      throws ArityException {
    Object[] args = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      args[i] = hostValueToEnsoNode.execute(arguments[i]);
    }
    if (arguments.length == 0) throw ArityException.create(1, -1, 0);
    return indirectInvokeMethodNode.execute(
        null,
        state,
        method,
        args[0],
        args,
        buildSchema(arguments.length),
        DefaultsExecutionMode.EXECUTE,
        ArgumentsExecutionMode.PRE_EXECUTED,
        TailStatus.NOT_TAIL,
        0);
  }
}
