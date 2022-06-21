package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.*;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.*;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
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
      BaseNode.TailStatus isTail,
      int thisArgumentPosition);

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
      int thisArgumentPosition,
      @CachedLibrary(limit = "10") MethodDispatchLibrary dispatch,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
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
          Context.get(this).getBuiltins().error().makeNoSuchMethodError(_this, symbol), this);
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
      int thisArgumentPosition,
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
  Stateful doWarning(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      WithWarnings _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @Cached IndirectInvokeMethodNode childDispatch) {
    arguments[thisArgumentPosition] = _this.getValue();
    ArrayRope<Warning> warnings = _this.getReassignedWarnings(this);
    Stateful result =
        childDispatch.execute(
            frame,
            state,
            symbol,
            _this.getValue(),
            arguments,
            schema,
            defaultsExecutionMode,
            argumentsExecutionMode,
            isTail,
            thisArgumentPosition);
    return new Stateful(result.getState(), WithWarnings.prependTo(result.getValue(), warnings));
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
      BaseNode.TailStatus isTail,
      int thisArgumentPosition) {
    throw _this;
  }

  @Specialization(
      guards = {
        "!methods.hasFunctionalDispatch(_this)",
        "!methods.hasSpecialDispatch(_this)",
        "polyglotCallType != NOT_SUPPORTED",
        "polyglotCallType != CONVERT_TO_TEXT"
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
      int thisArgumentPosition,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Bind("getPolyglotCallType(_this, symbol.getName(), interop)")
          HostMethodCallNode.PolyglotCallType polyglotCallType,
      @Cached ThunkExecutorNode argExecutor,
      @Cached HostMethodCallNode hostMethodCallNode,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    Object[] args = new Object[arguments.length - 1];
    for (int i = 0; i < arguments.length - 1; i++) {
      Stateful r = argExecutor.executeThunk(arguments[i + 1], state, BaseNode.TailStatus.NOT_TAIL);
      if (r.getValue() instanceof DataflowError) {
        return r;
      }
      state = r.getState();
      args[i] = r.getValue();
    }
    return new Stateful(
        state, hostMethodCallNode.execute(polyglotCallType, symbol.getName(), _this, args));
  }

  @Specialization(
      guards = {
        "!methods.hasFunctionalDispatch(_this)",
        "!methods.hasSpecialDispatch(_this)",
        "getPolyglotCallType(_this, symbol.getName(), interop) == CONVERT_TO_TEXT"
      })
  Stateful doConvertText(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "1") MethodDispatchLibrary textDispatch,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    try {
      String str = interop.asString(_this);
      Text txt = Text.create(str);
      Function function = textDispatch.getFunctionalDispatch(txt, symbol);
      arguments[0] = txt;
      return invokeFunctionNode.execute(
          function,
          frame,
          state,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, _this is guaranteed to be a string.");
    } catch (MethodDispatchLibrary.NoSuchMethodException e) {
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeNoSuchMethodError(_this, symbol), this);
    }
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
      int thisArgumentPosition,
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
