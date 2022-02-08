package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.source.SourceSection;
import java.util.UUID;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.*;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;
import org.enso.interpreter.runtime.state.Stateful;

@ImportStatic({HostMethodCallNode.PolyglotCallType.class, HostMethodCallNode.class})
public abstract class InvokeMethodNode extends BaseNode {
  private @Child InvokeFunctionNode invokeFunctionNode;
  private final ConditionProfile errorReceiverProfile = ConditionProfile.createCountingProfile();
  private final int argumentCount;

  /**
   * Creates a new node for method invocation.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param defaultsExecutionMode the defaulted arguments handling mode for this call
   * @param argumentsExecutionMode the arguments execution mode for this call
   * @return a new invoke method node
   */
  public static InvokeMethodNode build(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    return InvokeMethodNodeGen.create(schema, defaultsExecutionMode, argumentsExecutionMode);
  }

  InvokeMethodNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode) {
    this.invokeFunctionNode =
        InvokeFunctionNode.build(schema, defaultsExecutionMode, argumentsExecutionMode);
    this.argumentCount = schema.length;
  }

  @Override
  public void setTailStatus(TailStatus tailStatus) {
    super.setTailStatus(tailStatus);
    this.invokeFunctionNode.setTailStatus(tailStatus);
  }

  public abstract Stateful execute(
      VirtualFrame frame, Object state, UnresolvedSymbol symbol, Object _this, Object[] arguments);

  @Specialization(guards = "dispatch.hasFunctionalDispatch(_this)")
  Stateful doFunctionalDispatch(
      VirtualFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      @CachedLibrary(limit = "10") MethodDispatchLibrary dispatch,
      @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> ctx) {
    try {
      Function function = dispatch.getFunctionalDispatch(_this, symbol);
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (MethodDispatchLibrary.NoSuchMethodException e) {
      throw new PanicException(
          ctx.get().getBuiltins().error().makeNoSuchMethodError(_this, symbol), this);
    }
  }

  @Specialization
  Stateful doDataflowError(
      VirtualFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      DataflowError _this,
      Object[] arguments,
      @Cached DataflowErrorResolverNode dataflowErrorResolverNode) {
    Function function = dataflowErrorResolverNode.execute(symbol, _this);
    if (errorReceiverProfile.profile(function == null)) {
      return new Stateful(state, _this);
    } else {
      return invokeFunctionNode.execute(function, frame, state, arguments);
    }
  }

  @Specialization
  Stateful doPanicSentinel(
      VirtualFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      PanicSentinel _this,
      Object[] arguments) {
    throw _this;
  }

  @ExplodeLoop
  @Specialization(
      guards = {
        "!methods.hasFunctionalDispatch(_this)",
        "!methods.hasSpecialDispatch(_this)",
        "polyglotCallType != NOT_SUPPORTED",
        "polyglotCallType != CONVERT_TO_TEXT"
      })
  Stateful doPolyglot(
      VirtualFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Bind("getPolyglotCallType(_this, symbol.getName(), interop)")
          HostMethodCallNode.PolyglotCallType polyglotCallType,
      @Cached(value = "buildExecutors()") ThunkExecutorNode[] argExecutors,
      @Cached(value = "buildProfiles()", dimensions = 1) BranchProfile[] profiles,
      @Cached HostMethodCallNode hostMethodCallNode) {
    Object[] args = new Object[argExecutors.length];
    for (int i = 0; i < argExecutors.length; i++) {
      Stateful r = argExecutors[i].executeThunk(arguments[i + 1], state, TailStatus.NOT_TAIL);
      if (r.getValue() instanceof DataflowError) {
        profiles[i].enter();
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
      VirtualFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "1") MethodDispatchLibrary textDispatch,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> ctx) {
    try {
      String str = interop.asString(_this);
      Text txt = Text.create(str);
      Function function = textDispatch.getFunctionalDispatch(txt, symbol);
      arguments[0] = txt;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, _this is guaranteed to be a string.");
    } catch (MethodDispatchLibrary.NoSuchMethodException e) {
      throw new PanicException(
          ctx.get().getBuiltins().error().makeNoSuchMethodError(_this, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!methods.hasFunctionalDispatch(_this)",
        "!methods.hasSpecialDispatch(_this)",
        "getPolyglotCallType(_this, symbol.getName(), interop) == NOT_SUPPORTED"
      })
  Stateful doFallback(
      VirtualFrame frame,
      Object state,
      UnresolvedSymbol symbol,
      Object _this,
      Object[] arguments,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached AnyResolverNode anyResolverNode) {
    Function function = anyResolverNode.execute(symbol, _this);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Override
  public SourceSection getSourceSection() {
    Node parent = getParent();
    return parent == null ? null : parent.getSourceSection();
  }

  BranchProfile[] buildProfiles() {
    BranchProfile[] result = new BranchProfile[argumentCount - 1];
    for (int i = 0; i < argumentCount - 1; i++) {
      result[i] = BranchProfile.create();
    }
    return result;
  }

  ThunkExecutorNode[] buildExecutors() {
    ThunkExecutorNode[] result = new ThunkExecutorNode[argumentCount - 1];
    for (int i = 0; i < argumentCount - 1; i++) {
      result[i] = ThunkExecutorNode.build();
    }
    return result;
  }

  /**
   * Sets the expression ID of this node.
   *
   * @param id the expression ID to assign this node.
   */
  public void setId(UUID id) {
    invokeFunctionNode.setId(id);
  }
}
