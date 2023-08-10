package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import java.util.UUID;
import java.util.concurrent.locks.Lock;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.ConversionResolverNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.*;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

public abstract class InvokeConversionNode extends BaseNode {
  private @Child InvokeFunctionNode invokeFunctionNode;
  private @Child InvokeConversionNode childDispatch;
  private final int thatArgumentPosition;

  /**
   * Creates a new node for method invocation.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param defaultsExecutionMode the defaulted arguments handling mode for this call
   * @param argumentsExecutionMode the arguments execution mode for this call
   * @return a new invoke method node
   */
  public static InvokeConversionNode build(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thatArgumentPosition) {
    return InvokeConversionNodeGen.create(
        schema, defaultsExecutionMode, argumentsExecutionMode, thatArgumentPosition);
  }

  InvokeConversionNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thatArgumentPosition) {
    this.invokeFunctionNode =
        InvokeFunctionNode.build(schema, defaultsExecutionMode, argumentsExecutionMode);
    this.thatArgumentPosition = thatArgumentPosition;
  }

  @Override
  public void setTailStatus(TailStatus tailStatus) {
    super.setTailStatus(tailStatus);
    this.invokeFunctionNode.setTailStatus(tailStatus);
  }

  /**
   * @param self A target of the conversion. Should be a {@link Type} on which the {@code from}
   *     method is defined. If it is not a {@link Type}, "Invalid conversion target" panic is
   *     thrown.
   * @param that Source of the conversion. Can be arbitrary object, including polyglot values.
   * @param arguments Additional arguments passed to the conversion function.
   */
  public abstract Object execute(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments);

  static Type extractConstructor(Node thisNode, Object self) {
    if (self instanceof Type) {
      return (Type) self;
    } else {
      throw new PanicException(
          EnsoContext.get(thisNode).getBuiltins().error().makeInvalidConversionTarget(self),
          thisNode);
    }
  }

  Type extractConstructor(Object self) {
    return extractConstructor(this, self);
  }

  @Specialization(guards = {"dispatch.hasType(that)", "!dispatch.hasSpecialDispatch(that)"})
  Object doConvertFrom(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary dispatch,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            that, extractConstructor(self), dispatch.getType(that), conversion);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization
  Object doDataflowError(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      DataflowError that,
      Object[] arguments,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary dispatch,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.execute(
            extractConstructor(self),
            EnsoContext.get(this).getBuiltins().dataflowError(),
            conversion);
    if (function != null) {
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } else {
      return that;
    }
  }

  @Specialization
  Object doPanicSentinel(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      PanicSentinel that,
      Object[] arguments) {
    throw that;
  }

  @Specialization
  Object doWarning(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      WithWarnings that,
      Object[] arguments) {
    // Cannot use @Cached for childDispatch, because we need to call notifyInserted.
    if (childDispatch == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      Lock lock = getLock();
      lock.lock();
      try {
        if (childDispatch == null) {
          childDispatch =
              insert(
                  build(
                      invokeFunctionNode.getSchema(),
                      invokeFunctionNode.getDefaultsExecutionMode(),
                      invokeFunctionNode.getArgumentsExecutionMode(),
                      thatArgumentPosition));
          childDispatch.setTailStatus(getTailStatus());
          childDispatch.setId(invokeFunctionNode.getId());
          notifyInserted(childDispatch);
        }
      } finally {
        lock.unlock();
      }
    }
    Object value = that.getValue();
    arguments[thatArgumentPosition] = value;
    ArrayRope<Warning> warnings = that.getReassignedWarningsAsRope(this);
    try {
      Object result = childDispatch.execute(frame, state, conversion, self, value, arguments);
      return WithWarnings.appendTo(EnsoContext.get(this), result, warnings);
    } catch (TailCallException e) {
      throw new TailCallException(e, warnings.toArray(Warning[]::new));
    }
  }

  @Specialization(guards = "interop.isString(that)")
  Object doConvertText(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    try {
      String str = interop.asString(that);
      Text txt = Text.create(str);
      Function function =
          conversionResolverNode.expectNonNull(
              txt,
              extractConstructor(self),
              EnsoContext.get(this).getBuiltins().text(),
              conversion);
      arguments[0] = txt;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, that is guaranteed to be a string.");
    }
  }

  @Specialization(
      guards = {
        "!typesLib.hasType(that)",
        "!typesLib.hasSpecialDispatch(that)",
        "!interop.isTime(that)",
        "interop.isDate(that)",
      })
  Object doConvertDate(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary typesLib,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            that, extractConstructor(self), EnsoContext.get(this).getBuiltins().date(), conversion);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!typesLib.hasType(that)",
        "!typesLib.hasSpecialDispatch(that)",
        "interop.isTime(that)",
        "!interop.isDate(that)",
      })
  Object doConvertTime(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary typesLib,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            that,
            extractConstructor(self),
            EnsoContext.get(this).getBuiltins().timeOfDay(),
            conversion);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!typesLib.hasType(that)",
        "!typesLib.hasSpecialDispatch(that)",
        "interop.isTime(that)",
        "interop.isDate(that)",
      })
  Object doConvertDateTime(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary typesLib,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            that,
            extractConstructor(self),
            EnsoContext.get(this).getBuiltins().dateTime(),
            conversion);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!typesLib.hasType(that)",
        "!typesLib.hasSpecialDispatch(that)",
        "interop.isDuration(that)",
      })
  Object doConvertDuration(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary typesLib,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            that,
            extractConstructor(self),
            EnsoContext.get(this).getBuiltins().duration(),
            conversion);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!typesLib.hasType(thatMap)",
        "!typesLib.hasSpecialDispatch(thatMap)",
        "interop.hasHashEntries(thatMap)",
      })
  Object doConvertMap(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object thatMap,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary typesLib,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    Function function =
        conversionResolverNode.expectNonNull(
            thatMap,
            extractConstructor(self),
            EnsoContext.get(this).getBuiltins().map(),
            conversion);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!methods.hasType(that)",
        "!interop.isString(that)",
        "!methods.hasSpecialDispatch(that)"
      })
  Object doFallback(
      VirtualFrame frame,
      State state,
      UnresolvedConversion conversion,
      Object self,
      Object that,
      Object[] arguments,
      @Shared("typesLib") @CachedLibrary(limit = "10") TypesLibrary methods,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("conversionResolverNode") @Cached ConversionResolverNode conversionResolverNode) {
    var ctx = EnsoContext.get(this);
    var function =
        conversionResolverNode.execute(
            extractConstructor(self), ctx.getBuiltins().any(), conversion);
    if (function == null) {
      throw new PanicException(
          ctx.getBuiltins().error().makeNoSuchConversion(self, that, conversion), this);
    } else {
      return invokeFunctionNode.execute(function, frame, state, arguments);
    }
  }

  @Override
  public SourceSection getSourceSection() {
    Node parent = getParent();
    return parent == null ? null : parent.getSourceSection();
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
