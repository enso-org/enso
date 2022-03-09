package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.HostMethodCallNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
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
public abstract class IndirectInvokeConversionNode extends Node {

  /** @return a new indirect method invocation node */
  public static IndirectInvokeConversionNode build() {
    return IndirectInvokeConversionNodeGen.create();
  }

  public abstract Stateful execute(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object _this,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition);

  @Specialization(guards = "dispatch.canConvertFrom(that)")
  Stateful doConvertFrom(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object _this,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @CachedLibrary(limit = "10") MethodDispatchLibrary dispatch,
      @Cached ConditionProfile atomProfile,
      @Cached ConditionProfile atomConstructorProfile,
      @Cached IndirectInvokeFunctionNode indirectInvokeFunctionNode) {
    try {
      Function function =
          dispatch.getConversionFunction(
              that,
              InvokeConversionNode.extractConstructor(
                  this, _this, atomConstructorProfile, atomProfile),
              conversion);
      return indirectInvokeFunctionNode.execute(
          function,
          frame,
          state,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    } catch (MethodDispatchLibrary.NoSuchConversionException e) {
      throw new PanicException(
          Context.get(this)
              .getBuiltins()
              .error()
              .makeNoSuchConversionError(_this, that, conversion),
          this);
    }
  }

  @Specialization
  Stateful doDataflowError(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object _this,
      DataflowError that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @CachedLibrary(limit = "10") MethodDispatchLibrary dispatch,
      @Cached BranchProfile profile,
      @Cached ConditionProfile atomProfile,
      @Cached ConditionProfile atomConstructorProfile,
      @Cached IndirectInvokeFunctionNode indirectInvokeFunctionNode) {
    try {
      Function function =
          dispatch.getConversionFunction(
              that,
              InvokeConversionNode.extractConstructor(
                  this, _this, atomConstructorProfile, atomProfile),
              conversion);
      return indirectInvokeFunctionNode.execute(
          function,
          frame,
          state,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    } catch (MethodDispatchLibrary.NoSuchConversionException e) {
      profile.enter();
      return new Stateful(state, that);
    }
  }

  @Specialization
  Stateful doPanicSentinel(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object _this,
      PanicSentinel that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition) {
    throw that;
  }

  @Specialization
  Stateful doWarning(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object _this,
      WithWarnings that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @Cached IndirectInvokeConversionNode childDispatch) {
    arguments[thatArgumentPosition] = that.getValue();
    ArrayRope<Warning> warnings = that.getReassignedWarnings(this);
    Stateful result =
        childDispatch.execute(
            frame,
            state,
            conversion,
            _this,
            that.getValue(),
            arguments,
            schema,
            defaultsExecutionMode,
            argumentsExecutionMode,
            isTail,
            thatArgumentPosition);
    return new Stateful(result.getState(), WithWarnings.prependTo(result.getValue(), warnings));
  }

  @Specialization(guards = "interop.isString(that)")
  Stateful doConvertText(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object _this,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "1") MethodDispatchLibrary textDispatch,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached ConditionProfile atomProfile,
      @Cached ConditionProfile atomConstructorProfile,
      @Cached IndirectInvokeFunctionNode indirectInvokeFunctionNode) {
    try {
      String str = interop.asString(that);
      Text txt = Text.create(str);
      Function function =
          textDispatch.getConversionFunction(
              txt,
              InvokeConversionNode.extractConstructor(
                  this, _this, atomConstructorProfile, atomProfile),
              conversion);
      arguments[0] = txt;
      return indirectInvokeFunctionNode.execute(
          function,
          frame,
          state,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, that is guaranteed to be a string.");
    } catch (MethodDispatchLibrary.NoSuchConversionException e) {
      throw new PanicException(
          Context.get(this)
              .getBuiltins()
              .error()
              .makeNoSuchConversionError(_this, that, conversion),
          this);
    }
  }

  @Specialization(
      guards = {
        "!methods.canConvertFrom(that)",
        "!interop.isString(that)",
        "!methods.hasSpecialConversion(that)"
      })
  Stateful doFallback(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object _this,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @CachedLibrary(limit = "10") MethodDispatchLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop) {
    throw new PanicException(
        Context.get(this).getBuiltins().error().makeNoSuchConversionError(_this, that, conversion),
        this);
  }
}
