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
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoDate;
import org.enso.interpreter.runtime.data.EnsoDateTime;
import org.enso.interpreter.runtime.data.EnsoDuration;
import org.enso.interpreter.runtime.data.EnsoTimeOfDay;
import org.enso.interpreter.runtime.data.EnsoTimeZone;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
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

  InvokeFunctionNode getInvokeFunctionNode() {
    return invokeFunctionNode;
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

  /**
   * Resolves symbol to a Warning method, if possible.
   *
   * <p>A regular method dispatch logic will extract/append warnings of `self` before invoking the
   * actual method dispatch logic. This allows for ignoring complexity related to the presence of
   * warnings but prevents us from manipulating warnings directly in the Enso code (they have just
   * been removed). `resolveWarningFunction` will attempt to resolve the symbol in the Warning type
   * scope, if possible. Additionally, we check if under the non-warning `self`, the symbol would
   * resolve to `Any`. If not, it means that we should employ a regular method dispatch logic, due
   * to a method name clash. E.g. if some collection type Foo defines a `has_warnings` method, we
   * should dispatch the call to `Foo`'s `has_warning` rather than to a `Warning` or `Any`'s `one.
   *
   * @param self `self` argument that has some warnings
   * @param symbol symbol to be resolved
   * @param types TypesLibrary instance
   * @param warnings WarningsLibrary instance
   * @return resolved Warning method to be called
   */
  Function resolveWarningFunction(
      Object self, UnresolvedSymbol symbol, TypesLibrary types, WarningsLibrary warnings) {
    Object selfWithoutWarnings;
    try {
      selfWithoutWarnings = warnings.removeWarnings(self);
    } catch (UnsupportedMessageException e) {
      throw CompilerDirectives.shouldNotReachHere(
          "`self` object should have some warnings when calling `" + symbol.getName() + "` method",
          e);
    }

    Type typeOfSymbol = symbol.resolveDeclaringType(types.getType(selfWithoutWarnings));
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    if (typeOfSymbol == builtins.any()) {
      return symbol
          .getScope()
          .lookupMethodDefinition(builtins.warning().getEigentype(), symbol.getName());
    }

    return null;
  }

  Object[] argumentsWithExplicitSelf(FunctionSchema cachedSchema, Object[] arguments) {
    Object[] arguments1;
    if (!cachedSchema.isFullyApplied()) {
      arguments1 = new Object[cachedSchema.getArgumentsCount()];
      System.arraycopy(arguments, 0, arguments1, 1, arguments.length);
      arguments1[0] = arguments[0];
    } else {
      arguments1 = arguments;
    }
    return arguments1;
  }

  public InvokeFunctionNode buildInvokeFunctionWithSelf() {
    int length = invokeFunctionNode.getSchema().length;
    CallArgumentInfo[] schema = new CallArgumentInfo[length + 1];
    System.arraycopy(invokeFunctionNode.getSchema(), 0, schema, 1, length);
    schema[0] = new CallArgumentInfo();
    return InvokeFunctionNode.build(
        schema,
        invokeFunctionNode.getDefaultsExecutionMode(),
        invokeFunctionNode.getArgumentsExecutionMode());
  }

  @Specialization(
      guards = {
        "warnings.hasWarnings(self)",
        "resolvedFunction != null",
        "resolvedFunction.getSchema() == cachedSchema"
      })
  Object doWarningsCustom(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Cached("resolveWarningFunction(self, symbol, types, warnings)") Function resolvedFunction,
      @Cached("resolvedFunction.getSchema()") FunctionSchema cachedSchema,
      @Cached("buildInvokeFunctionWithSelf()") InvokeFunctionNode warningFunctionNode) {
    // Warning's builtin type methods are static meaning that they have a synthetic `self`
    // parameter. However, the constructed `InvokeFunctionNode` is missing that
    // information and the function, if called with `arguments`, will not be fully applied.
    // Hence, the synthetic construction of a new `InvokeFunctionNode` with the updated schema
    // and call including an additional, dummy, argument.
    Object[] arguments1 = argumentsWithExplicitSelf(cachedSchema, arguments);
    return warningFunctionNode.execute(resolvedFunction, frame, state, arguments1);
  }

  @Specialization(guards = "warnings.hasWarnings(self)")
  Object doWarning(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") WarningsLibrary warnings) {
    Object selfWithoutWarnings;
    Warning[] arrOfWarnings;
    try {
      selfWithoutWarnings = warnings.removeWarnings(self);
      arrOfWarnings = warnings.getWarnings(self, this);
    } catch (UnsupportedMessageException e) {
      // Can't throw `CompilerDirectives.shouldNotReachHere` as it crashes native-image build
      throw new IllegalStateException(e);
    }

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
          childDispatch.setId(invokeFunctionNode.getId());
          notifyInserted(childDispatch);
        }
      } finally {
        lock.unlock();
      }
    }

    arguments[thisArgumentPosition] = selfWithoutWarnings;

    Object result = childDispatch.execute(frame, state, symbol, selfWithoutWarnings, arguments);
    return WithWarnings.appendTo(result, arrOfWarnings);
  }

  @ExplodeLoop
  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
      if (r instanceof DataflowError) {
        profiles[i].enter();
        return r;
      } else if (warnings.hasWarnings(r)) {
        warningProfiles[i].enter();
        anyWarnings = true;
        try {
          accumulatedWarnings = accumulatedWarnings.append(warnings.getWarnings(r, this));
          args[i] = warnings.removeWarnings(r);
        } catch (UnsupportedMessageException e) {
          throw new IllegalStateException(e);
        }
      } else {
        args[i] = r;
      }
    }
    Object res = hostMethodCallNode.execute(polyglotCallType, symbol.getName(), self, args);
    if (anyWarnings) {
      anyWarningsProfile.enter();
      res = WithWarnings.appendTo(res, accumulatedWarnings);
    }
    return res;
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    var arrayType = ctx.getBuiltins().array();
    var function = methodResolverNode.expectNonNull(self, arrayType, symbol);
    arguments[0] = self;
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop, methodResolverNode) == CONVERT_TO_HASH_MAP",
      })
  Object doConvertHashMap(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "10") TypesLibrary types,
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    var hashMapType = ctx.getBuiltins().map();
    var function = methodResolverNode.expectNonNull(self, hashMapType, symbol);
    arguments[0] = self;
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Cached MethodResolverNode methodResolverNode) {
    var ctx = EnsoContext.get(this);
    try {
      var hostLocalDate = interop.asDate(self);
      var date = new EnsoDate(hostLocalDate);
      Function function = methodResolverNode.expectNonNull(date, ctx.getBuiltins().date(), symbol);

      arguments[0] = date;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethod(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethod(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethod(self, symbol), this);
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
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethod(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethod(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
      throw new PanicException(ctx.getBuiltins().error().makeNoSuchMethod(self, symbol), this);
    }
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
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
      @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Cached MethodResolverNode resolverNode) {
    var ctx = EnsoContext.get(this);
    Function function = resolverNode.expectNonNull(self, ctx.getBuiltins().function(), symbol);
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
    if (childDispatch != null) {
      childDispatch.setId(id);
    }
  }
}
