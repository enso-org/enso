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
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
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
      Object self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition);

  @Specialization(guards = {"dispatch.hasType(self)", "!dispatch.hasSpecialDispatch(self)"})
  Stateful doFunctionalDispatch(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @CachedLibrary(limit = "10") TypesLibrary dispatch,
      @Cached MethodResolverNode methodResolverNode,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    Function function = methodResolverNode.expectNonNull(self, dispatch.getType(self), symbol);
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

  @Specialization
  Stateful doDataflowError(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      DataflowError self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @Cached MethodResolverNode methodResolverNode,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode,
      @Cached ConditionProfile profile) {
    Function function =
        methodResolverNode.execute(Context.get(this).getBuiltins().dataflowError(), symbol);
    if (profile.profile(function == null)) {
      return new Stateful(state, self);
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
      WithWarnings self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @Cached IndirectInvokeMethodNode childDispatch) {
    arguments[thisArgumentPosition] = self.getValue();
    ArrayRope<Warning> warnings = self.getReassignedWarnings(this);
    Stateful result =
        childDispatch.execute(
            frame,
            state,
            symbol,
            self.getValue(),
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
      PanicSentinel self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition) {
    throw self;
  }

  @Specialization(
      guards = {
        "!methods.hasType(self)",
        "!methods.hasSpecialDispatch(self)",
        "polyglotCallType != NOT_SUPPORTED",
        "polyglotCallType != CONVERT_TO_TEXT"
      })
  Stateful doPolyglot(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @CachedLibrary(limit = "10") TypesLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Bind("getPolyglotCallType(self, symbol, interop)")
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
        state, hostMethodCallNode.execute(polyglotCallType, symbol.getName(), self, args));
  }

  @Specialization(
      guards = {
        "!methods.hasType(self)",
        "!methods.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_TEXT"
      })
  Stateful doConvertText(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @CachedLibrary(limit = "10") TypesLibrary methods,
      @Cached MethodResolverNode methodResolverNode,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    try {
      var str = interop.asString(self);
      var text = Text.create(str);
      var ctx = Context.get(this);
      var textType = ctx.getBuiltins().text();
      var function = methodResolverNode.expectNonNull(text, textType, symbol);
      arguments[0] = text;
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
      throw new IllegalStateException("Impossible, self is guaranteed to be a string.");
    }
  }

  @ExplodeLoop
  @Specialization(
      guards = {
        "!methods.hasType(self)",
        "!methods.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == NOT_SUPPORTED"
      })
  Stateful doFallback(
      MaterializedFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thisArgumentPosition,
      @CachedLibrary(limit = "10") TypesLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Bind("getPolyglotCallType(self, symbol, interop)")
          HostMethodCallNode.PolyglotCallType polyglotCallType,
      @Cached MethodResolverNode methodResolverNode,
      @Cached IndirectInvokeFunctionNode invokeFunctionNode) {
    Function function =
        methodResolverNode.expectNonNull(self, Context.get(this).getBuiltins().any(), symbol);
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
