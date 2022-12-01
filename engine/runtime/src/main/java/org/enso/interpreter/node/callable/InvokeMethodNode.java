package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.HostMethodCallNode;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoDate;
import org.enso.interpreter.runtime.data.EnsoDateTime;
import org.enso.interpreter.runtime.data.EnsoDuration;
import org.enso.interpreter.runtime.data.EnsoTimeOfDay;
import org.enso.interpreter.runtime.data.EnsoTimeZone;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.UUID;
import java.util.concurrent.locks.Lock;

@ImportStatic({HostMethodCallNode.PolyglotCallType.class, HostMethodCallNode.class})
public abstract class InvokeMethodNode extends BaseNode {
  private @Child InvokeFunctionNode invokeFunctionNode;
  private final ConditionProfile errorReceiverProfile = ConditionProfile.createCountingProfile();
  private @Child InvokeMethodNode childDispatch;
  private final int argumentCount;
  private final int thisArgumentPosition;

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
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thisArgumentPosition) {
    return InvokeMethodNodeGen.create(
        schema, defaultsExecutionMode, argumentsExecutionMode, thisArgumentPosition);
  }

  InvokeMethodNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thisArgumentPosition) {
    this.invokeFunctionNode =
        InvokeFunctionNode.build(schema, defaultsExecutionMode, argumentsExecutionMode);
    this.argumentCount = schema.length;
    this.thisArgumentPosition = thisArgumentPosition;
  }

  @Override
  public void setTailStatus(TailStatus tailStatus) {
    super.setTailStatus(tailStatus);
    this.invokeFunctionNode.setTailStatus(tailStatus);
    if (childDispatch != null) {
      childDispatch.setTailStatus(tailStatus);
    }
  }

  public abstract Object execute(
      VirtualFrame frame, State state, UnresolvedSymbol symbol, Object self, Object[] arguments);

  @Specialization(guards = {"dispatch.hasType(self)", "!dispatch.hasSpecialDispatch(self)"})
  Object doFunctionalDispatch(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary dispatch,
      @Cached MethodResolverNode methodResolverNode) {
    Function function = methodResolverNode.expectNonNull(self, dispatch.getType(self), symbol);
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization
  Object doDataflowError(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      DataflowError self,
      Object[] arguments,
      @Cached MethodResolverNode methodResolverNode) {
    Function function =
        methodResolverNode.execute(EnsoContext.get(this).getBuiltins().dataflowError(), symbol);
    if (errorReceiverProfile.profile(function == null)) {
      return self;
    } else {
      return invokeFunctionNode.execute(function, frame, state, arguments);
    }
  }

  @Specialization
  Object doPanicSentinel(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      PanicSentinel self,
      Object[] arguments) {
    throw self;
  }

  @Specialization
  Object doWarning(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      WithWarnings self,
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
                      thisArgumentPosition));
          childDispatch.setTailStatus(getTailStatus());
          notifyInserted(childDispatch);
        }
      } finally {
        lock.unlock();
      }
    }

    arguments[thisArgumentPosition] = self.getValue();
    ArrayRope<Warning> warnings = self.getReassignedWarnings(this);
    Object result = childDispatch.execute(frame, state, symbol, self.getValue(), arguments);
    return WithWarnings.prependTo(result, warnings);
  }

  @ExplodeLoop
  @Specialization(
      guards = {
        "!methods.hasType(self)",
        "!methods.hasSpecialDispatch(self)",
        "polyglotCallType.isInteropLibrary()",
      })
  Object doPolyglot(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode preResolveMethod,
      @Bind("getPolyglotCallType(self, symbol, interop, preResolveMethod)")
          HostMethodCallNode.PolyglotCallType polyglotCallType,
      @Cached(value = "buildExecutors()") ThunkExecutorNode[] argExecutors,
      @Cached(value = "buildProfiles()", dimensions = 1) BranchProfile[] profiles,
      @Cached(value = "buildProfiles()", dimensions = 1) BranchProfile[] warningProfiles,
      @Cached BranchProfile anyWarningsProfile,
      @Cached HostMethodCallNode hostMethodCallNode) {
    Object[] args = new Object[argExecutors.length];
    boolean anyWarnings = false;
    ArrayRope<Warning> accumulatedWarnings = new ArrayRope<>();
    for (int i = 0; i < argExecutors.length; i++) {
       var r = argExecutors[i].executeThunk(arguments[i + 1], state, TailStatus.NOT_TAIL);
       args[i] = r;
      if (r instanceof DataflowError) {
        profiles[i].enter();
        return r;
      } else if (r instanceof WithWarnings w) {
        warningProfiles[i].enter();
        anyWarnings = true;
        accumulatedWarnings =
            accumulatedWarnings.append(w.getReassignedWarnings(this));
        args[i] = w.getValue();
      }
    }
    Object res = hostMethodCallNode.execute(polyglotCallType, symbol.getName(), self, args);
    if (anyWarnings) {
      anyWarningsProfile.enter();
      res = WithWarnings.prependTo(res, accumulatedWarnings);
    }
    return res;
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_TEXT"
      })
  Object doConvertText(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @Cached MethodResolverNode methodResolverNode) {
    try {
      var str = interop.asString(self);
      var text = Text.create(str);
      var ctx = EnsoContext.get(this);
      var textType = ctx.getBuiltins().text();
      var function = methodResolverNode.expectNonNull(text, textType, symbol);
      arguments[0] = text;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, self is guaranteed to be a string.");
    }
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop, methodResolverNode) == CONVERT_TO_ARRAY",
      })
  Object doConvertArray(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    var arrayType = ctx.getBuiltins().array();
    var function = methodResolverNode.expectNonNull(self, arrayType, symbol);
    arguments[0] = self;
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_DATE"
      })
  Object doConvertDate(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    try {
      var hostLocalDate = interop.asDate(self);
      var date = new EnsoDate(hostLocalDate);
      Function function = methodResolverNode.expectNonNull(date, ctx.getBuiltins().date(), symbol);

      arguments[0] = date;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethodError(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_DATE_TIME"
      })
  Object doConvertDateTime(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    try {
      var hostLocalDate = interop.asDate(self);
      var hostLocalTime = interop.asTime(self);
      var dateTime = new EnsoDateTime(dateTime(hostLocalDate, hostLocalTime));
      Function function =
          methodResolverNode.expectNonNull(dateTime, ctx.getBuiltins().dateTime(), symbol);

      arguments[0] = dateTime;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethodError(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_DURATION"
      })
  Object doConvertDuration(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    try {
      var duration = interop.asDuration(self);
      var ensoDuration = new EnsoDuration(duration);
      Function function =
          methodResolverNode.expectNonNull(ensoDuration, ctx.getBuiltins().duration(), symbol);
      arguments[0] = ensoDuration;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethodError(self, symbol), this);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private ZonedDateTime dateTime(LocalDate date, LocalTime time) {
    return date.atTime(time).atZone(ZoneId.systemDefault());
  }

  @CompilerDirectives.TruffleBoundary
  private ZonedDateTime dateTime(LocalDate date, LocalTime time, ZoneId zone) {
    return date.atTime(time).atZone(zone);
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_ZONED_DATE_TIME"
      })
  Object doConvertZonedDateTime(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    try {
      var hostLocalDate = interop.asDate(self);
      var hostLocalTime = interop.asTime(self);
      var hostZone = interop.asTimeZone(self);
      var dateTime = new EnsoDateTime(dateTime(hostLocalDate, hostLocalTime, hostZone));
      Function function =
          methodResolverNode.expectNonNull(dateTime, ctx.getBuiltins().dateTime(), symbol);
      arguments[0] = dateTime;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethodError(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_TIME_ZONE"
      })
  Object doConvertZone(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    try {
      var hostZone = interop.asTimeZone(self);
      var dateTime = new EnsoTimeZone(hostZone);
      Function function =
          methodResolverNode.expectNonNull(dateTime, ctx.getBuiltins().timeZone(), symbol);
      arguments[0] = dateTime;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethodError(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_TIME_OF_DAY"
      })
  Object doConvertTimeOfDay(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    try {
      var hostLocalTime = interop.asTime(self);
      var dateTime = new EnsoTimeOfDay(hostLocalTime);
      Function function =
          methodResolverNode.expectNonNull(dateTime, ctx.getBuiltins().timeOfDay(), symbol);
      arguments[0] = dateTime;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethodError(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!methods.hasType(self)",
        "!methods.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == NOT_SUPPORTED"
      })
  Object doFallback(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary methods,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached MethodResolverNode anyResolverNode) {
    var ctx = EnsoContext.get(this);
    Function function = anyResolverNode.expectNonNull(self, ctx.getBuiltins().any(), symbol);
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
