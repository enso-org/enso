package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.*;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.state.Stateful;

@GenerateUncached
@ReportPolymorphism
@ImportStatic({HostMethodCallNode.PolyglotCallType.class, HostMethodCallNode.class})
public abstract class IndirectInvokeMethodNode extends Node {

  /** @return a new indirect method invocation node */
  public static IndirectInvokeMethodNode build() {
    return IndirectInvokeMethodNodeGen.create();
  }

  public abstract Stateful execute(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail);

  @Specialization(guards = "dispatch.hasFunctionalDispatch(_this)")
  Stateful doFunctionalDispatch(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @CachedLibrary(limit = "10") MethodDispatchLibrary dispatch,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode,
      @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> ctx) {
    try {
      Function function = dispatch.getFunctionalDispatch(_this, symbol);
      return invokeFunctionNode.execute(
          function,
          frame,
          state,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    } catch (MethodDispatchLibrary.NoSuchMethodException e) {
      throw new PanicException(
          ctx.get().getBuiltins().error().makeNoSuchMethodError(_this, symbol), this);
    }
  }

  @Specialization
  Stateful doDataflowError(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      DataflowError _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @Cached DataflowErrorResolverNode dataflowErrorResolverNode,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode,
      @Cached ConditionProfile profile) {
    Function function = dataflowErrorResolverNode.execute(symbol, _this);
    if (profile.profile(function == null)) {
      return new Stateful(state, _this);
    } else {
      return invokeFunctionNode.execute(
          function,
          frame,
          state,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    }
  }

  @Specialization
  Stateful doPanicSentinel(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      PanicSentinel _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail) {
    throw _this;
  }

  @Specialization(
      guards = {
        "!methods.hasFunctionalDispatch(_this)",
        "!methods.hasSpecialDispatch(_this)",
        "polyglotCallType != NOT_SUPPORTED"
      })
  Stateful doPolyglot(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Bind("getPolyglotCallType(_this, symbol.getName(), interop)")
          HostMethodCallNode.PolyglotCallType polyglotCallType,
      @Cached ThunkExecutorNode argExecutor,
      @Cached AnyResolverNode anyResolverNode,
      @Cached HostMethodCallNode hostMethodCallNode,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {

    Object[] args = new Object[arguments.length - 1];
    for (int i = 0; i < arguments.length - 1; i++) {
      Stateful r = argExecutor.executeThunk(arguments[i + 1], state, BaseNode.TailStatus.NOT_TAIL);
      state = r.getState();
      args[i] = r.getValue();
    }
    return new Stateful(
        state, hostMethodCallNode.execute(polyglotCallType, symbol.getName(), _this, args));
  }

  @ExplodeLoop
  @Specialization(
      guards = {
        "!methods.hasFunctionalDispatch(_this)",
        "!methods.hasSpecialDispatch(_this)",
        "getPolyglotCallType(_this, symbol.getName(), interop) == NOT_SUPPORTED"
      })
  Stateful doFallback(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Bind("getPolyglotCallType(_this, symbol.getName(), interop)")
          HostMethodCallNode.PolyglotCallType polyglotCallType,
      @Cached ThunkExecutorNode argExecutor,
      @Cached AnyResolverNode anyResolverNode,
      @Cached HostMethodCallNode hostMethodCallNode,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    Function function = anyResolverNode.execute(symbol, _this);
    return invokeFunctionNode.execute(
        function,
        frame,
        state,
        arguments,
        schema,
        defaultsExecutionMode,
        argumentsExecutionMode,
        isTail);
  }
}
