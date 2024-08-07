package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
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
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.runtime.warning.AppendWarningNode;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
import org.enso.interpreter.runtime.warning.WithWarnings;

@GenerateUncached
@ReportPolymorphism
@ImportStatic({HostMethodCallNode.PolyglotCallType.class, HostMethodCallNode.class})
abstract class IndirectInvokeConversionNode extends Node {

  /**
   * @return a new indirect method invocation node
   */
  static IndirectInvokeConversionNode build() {
    return IndirectInvokeConversionNodeGen.create();
  }

  abstract Object execute(
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

  static boolean hasType(TypeOfNode typeOfNode, Object value) {
    return typeOfNode.execute(value) instanceof Type;
  }

  @Specialization(guards = {"hasType(typeOfNode, that)"})
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
      @Shared("typeOfNode") @Cached TypeOfNode typeOfNode,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode,
      @Shared("indirectInvokeFunctionNode") @Cached
          IndirectInvokeFunctionNode indirectInvokeFunctionNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            that,
            InvokeConversionNode.extractType(this, self),
            (Type) typeOfNode.execute(that),
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
      @Cached IndirectInvokeConversionNode childDispatch,
      @Cached AppendWarningNode appendWarningNode,
      @CachedLibrary(limit = "3") WarningsLibrary warnsLib) {
    arguments[thatArgumentPosition] = that.getValue();
    EnsoHashMap warnings;
    try {
      warnings = warnsLib.getWarnings(that, false);
    } catch (UnsupportedMessageException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
    }
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
    return appendWarningNode.executeAppend(null, result, warnings);
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

  @Specialization(guards = {"hasType(methods, that)", "!interop.isString(that)"})
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
      @Shared("typeOfNode") @Cached TypeOfNode methods,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    throw new PanicException(
        EnsoContext.get(this).getBuiltins().error().makeNoSuchConversion(self, that, conversion),
        this);
  }
}
