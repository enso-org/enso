package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

/** A helper node to handle conversion application for the interop library. */
@GenerateUncached
@NodeInfo(description = "Helper node to handle conversion application through the interop library.")
public abstract class InteropConversionCallNode extends Node {

  public static InteropConversionCallNode build() {
    return InteropConversionCallNodeGen.create();
  }

  public abstract Object execute(UnresolvedConversion conversion, Object state, Object[] arguments)
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
  InvokeConversionNode buildInvoker(int length) {
    CallArgumentInfo[] args = buildSchema(length);
    return InvokeConversionNode.build(
        args, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.PRE_EXECUTED, 1);
  }

  Context getContext() {
    return Context.get(this);
  }

  @Specialization(
      guards = {"!getContext().isInlineCachingDisabled()", "arguments.length == cachedArgsLength"},
      limit = Constants.CacheSizes.FUNCTION_INTEROP_LIBRARY)
  @ExplodeLoop
  Object callCached(
      UnresolvedConversion conversion,
      Object state,
      Object[] arguments,
      @Cached("arguments.length") int cachedArgsLength,
      @Cached("buildInvoker(cachedArgsLength)") InvokeConversionNode invokerNode,
      @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode)
      throws ArityException {
    Object[] args = new Object[cachedArgsLength];
    for (int i = 0; i < cachedArgsLength; i++) {
      args[i] = hostValueToEnsoNode.execute(arguments[i]);
    }
    if (cachedArgsLength < 2) throw ArityException.create(2, -1, cachedArgsLength);
    return invokerNode.execute(null, state, conversion, args[0], args[1], args).getValue();
  }

  @Specialization(replaces = "callCached")
  Object callUncached(
      UnresolvedConversion conversion,
      Object state,
      Object[] arguments,
      @Cached IndirectInvokeConversionNode indirectInvokeConversionNode,
      @Cached("build()") HostValueToEnsoNode hostValueToEnsoNode)
      throws ArityException {
    Object[] args = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      args[i] = hostValueToEnsoNode.execute(arguments[i]);
    }
    if (arguments.length < 2) throw ArityException.create(2, -1, arguments.length);
    return indirectInvokeConversionNode
        .execute(
            null,
            state,
            conversion,
            args[0],
            args[1],
            args,
            buildSchema(arguments.length),
            DefaultsExecutionMode.EXECUTE,
            ArgumentsExecutionMode.PRE_EXECUTED,
            TailStatus.NOT_TAIL,
            1)
        .getValue();
  }
}
