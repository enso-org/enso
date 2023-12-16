package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.IndirectInvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.ConversionResolverNode;
import org.enso.interpreter.node.callable.resolver.HostMethodCallNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.*;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@GenerateUncached
@ReportPolymorphism
@ImportStatic({HostMethodCallNode.PolyglotCallType.class, HostMethodCallNode.class})
public abstract class IndirectInvokeConversionNode extends Node {

  /**
   * @return a new indirect method invocation node
   */
  public static IndirectInvokeConversionNode build() {
    return IndirectInvokeConversionNodeGen.create();
  }

  public abstract Object execute(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition);

  @Specialization(guards = {"typesLib.hasType(that)", "!typesLib.hasSpecialDispatch(that)"})
  Object doConvertFrom(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary typesLib,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode,
      @Shared("indirectInvokeFunctionNode") @Cached
          IndirectInvokeFunctionNode indirectInvokeFunctionNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            that, InvokeConversionNode.extractType(this, self), typesLib.getType(that), conversion);
    return indirectInvokeFunctionNode.execute(
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
  Object doDataflowError(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      DataflowError that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @Shared("indirectInvokeFunctionNode") @Cached
          IndirectInvokeFunctionNode indirectInvokeFunctionNode,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.execute(
            InvokeConversionNode.extractType(this, self),
            EnsoContext.get(this).getBuiltins().dataflowError(),
            conversion);
    if (function != null) {
      return indirectInvokeFunctionNode.execute(
          function,
          frame,
          state,
          arguments,
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          isTail);
    } else {
      return that;
    }
  }

  @Specialization
  Object doPanicSentinel(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
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
  Object doWarning(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      WithWarnings that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @Cached IndirectInvokeConversionNode childDispatch) {
    arguments[thatArgumentPosition] = that.getValue();
    ArrayRope<Warning> warnings = that.getReassignedWarningsAsRope(this);
    Object result =
        childDispatch.execute(
            frame,
            state,
            conversion,
            self,
            that.getValue(),
            arguments,
            schema,
            defaultsExecutionMode,
            argumentsExecutionMode,
            isTail,
            thatArgumentPosition);
    return WithWarnings.appendTo(EnsoContext.get(this), result, warnings);
  }

  @Specialization(guards = "interop.isString(that)")
  Object doConvertText(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode,
      @Shared("indirectInvokeFunctionNode") @Cached
          IndirectInvokeFunctionNode indirectInvokeFunctionNode) {
    try {
      String str = interop.asString(that);
      Text txt = Text.create(str);
      Function function =
          conversionResolverNode.expectNonNull(
              txt,
              InvokeConversionNode.extractType(this, self),
              EnsoContext.get(this).getBuiltins().text(),
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
      throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);
    }
  }

  @Specialization(
      guards = {
        "!methods.hasType(that)",
        "!interop.isString(that)",
        "!methods.hasSpecialDispatch(that)"
      })
  Object doFallback(
      MaterializedFrame frame,
      Object state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      BaseNode.TailStatus isTail,
      int thatArgumentPosition,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary methods,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    throw new PanicException(
        EnsoContext.get(this).getBuiltins().error().makeNoSuchConversion(self, that, conversion),
        this);
  }
}
