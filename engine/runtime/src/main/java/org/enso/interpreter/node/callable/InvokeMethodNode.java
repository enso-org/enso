package org.enso.interpreter.node.callable;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.locks.Lock;

import org.enso.interpreter.Constants.Names;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.HostMethodCallNode;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.control.TailCallException;
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

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.NonIdempotent;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import com.oracle.truffle.api.source.SourceSection;

@ImportStatic({HostMethodCallNode.PolyglotCallType.class, HostMethodCallNode.class})
public abstract class InvokeMethodNode extends BaseNode {
  protected static final int CACHE_SIZE = 10;
  private @Child InvokeFunctionNode invokeFunctionNode;
  /**
   * A node that is created specifically for cases when a static method is called on {@code Any}. In
   * such cases, we need to modify the number of passed arguments, therefore, a new {@link
   * InvokeFunctionNode} has to be created.
   */
  private @Child InvokeFunctionNode invokeAnyStaticFunctionNode;

  private final CountingConditionProfile errorReceiverProfile = CountingConditionProfile.create();
  private @Child InvokeMethodNode childDispatch;

  private final int argumentCount;
  private final int thisArgumentPosition;
  private final boolean onBoundary;

  /**
   * Creates a new node for method invocation.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param defaultsExecutionMode the defaulted arguments handling mode for this call
   * @param argumentsExecutionMode the arguments execution mode for this call
   * @param thisArgumentPosition position
   * @param onBoundary shall we emit plain {@code PanicException} or also attach {@code UnknownIdentifierException} cause
   * @return a new invoke method node
   */
  public static InvokeMethodNode build(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thisArgumentPosition, boolean onBoundary) {
    return InvokeMethodNodeGen.create(
        schema, defaultsExecutionMode, argumentsExecutionMode, thisArgumentPosition, onBoundary);
  }

  InvokeMethodNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thisArgumentPosition, boolean onBoundary
  ) {
    this.invokeFunctionNode =
        InvokeFunctionNode.build(schema, defaultsExecutionMode, argumentsExecutionMode);
    this.argumentCount = schema.length;
    this.thisArgumentPosition = thisArgumentPosition;
    this.onBoundary = onBoundary;
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

  @NonIdempotent
  boolean isAnyEigenType(Type type) {
    return EnsoContext.get(this).getBuiltins().any().getEigentype() == type;
  }

  @Specialization(
      guards = {
        "typesLibrary.hasType(self)",
        "!typesLibrary.hasSpecialDispatch(self)",
        "cachedSymbol == symbol",
        "cachedSelfTpe == typesLibrary.getType(self)",
        "!isAnyEigenType(cachedSelfTpe)",
        "function != null"
      },
      limit = "CACHE_SIZE")
  Object doFunctionalDispatchCachedSymbol(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary typesLibrary,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("typesLibrary.getType(self)") Type cachedSelfTpe,
      @Cached("resolveFunction(cachedSymbol, cachedSelfTpe, methodResolverNode)")
          Function function) {
    assert arguments.length == invokeFunctionNode.getSchema().length;
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  Function resolveFunction(
      UnresolvedSymbol symbol, Type selfTpe, MethodResolverNode methodResolverNode) {
    Function function = methodResolverNode.execute(selfTpe, symbol);
    if (function == null) {
      return null;
    }

    RootNode where = function.getCallTarget().getRootNode();
    // If both Any and the type where `function` is declared, define `symbol`
    // and the method is invoked statically, i.e. type of self is the eigentype,
    // then we want to disambiguate method resolution by always resolved to the one in Any.
    EnsoContext ctx = EnsoContext.get(this);
    if (where instanceof MethodRootNode node && typeCanOverride(node, ctx)) {
      Type any = ctx.getBuiltins().any();
      Function anyFun = symbol.getScope().lookupMethodDefinition(any, symbol.getName());
      if (anyFun != null) {
        function = anyFun;
      }
    }
    return function;
  }

  private boolean typeCanOverride(MethodRootNode node, EnsoContext ctx) {
    Type methodOwnerType = node.getType();
    Builtins builtins = ctx.getBuiltins();
    Type any = builtins.any();
    Type warning = builtins.warning();
    Type panic = builtins.panic();
    return methodOwnerType.isEigenType()
        && builtins.nothing() != methodOwnerType
        && any.getEigentype() != methodOwnerType
        && panic.getEigentype() != methodOwnerType
        && warning.getEigentype() != methodOwnerType;
  }

  @Specialization(
      replaces = "doFunctionalDispatchCachedSymbol",
      guards = {"typesLibrary.hasType(self)", "!typesLibrary.hasSpecialDispatch(self)"})
  Object doFunctionalDispatchUncachedSymbol(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary typesLibrary,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
    Type selfTpe = typesLibrary.getType(self);
    Function function = resolveFunction(symbol, selfTpe, methodResolverNode);
    if (function == null) {
      var cause = onBoundary ? UnknownIdentifierException.create(symbol.getName()) : null;
      var payload = EnsoContext.get(this).getBuiltins().error().makeNoSuchMethod(self, symbol);
      throw new PanicException(payload, cause, this);
    }
    var resolvedFuncArgCount = function.getSchema().getArgumentsCount();
    CallArgumentInfo[] invokeFuncSchema = invokeFunctionNode.getSchema();
    long argsWithDefaultValCount = 0;
    for (var argDef : function.getSchema().getArgumentInfos()) {
      if (argDef.hasDefaultValue()) {
        argsWithDefaultValCount++;
      }
    }
    // Static method calls on Any are resolved to `Any.type.method`. Such methods take one additional
    // self argument (with Any.type) as opposed to static method calls resolved on any other
    // types. This case is handled in the following block.
    boolean shouldPrependSyntheticSelfArg = resolvedFuncArgCount - argsWithDefaultValCount == arguments.length + 1;
    if (isAnyEigenType(selfTpe) && shouldPrependSyntheticSelfArg) {
      // function is a static method on Any, so the first two arguments in `invokeFuncSchema`
      // represent self arguments.
      boolean selfArgSpecified = false;
      if (invokeFuncSchema.length > 1) {
        selfArgSpecified =
            invokeFuncSchema[1].getName() != null && invokeFuncSchema[1].getName().equals(Names.SELF_ARGUMENT);
      }

      if (selfArgSpecified) {
        // If there is a self named argument in the method call, we fall back to the old
        // behavior - there will be no prepended Any.type self argument
        assert arguments.length == invokeFuncSchema.length;
        return invokeFunctionNode.execute(function, frame, state, arguments);
      }

      Object[] argsWithPrependedSelf = new Object[arguments.length + 1];
      argsWithPrependedSelf[0] = EnsoContext.get(this).getBuiltins().any().getEigentype();
      System.arraycopy(arguments, 0, argsWithPrependedSelf, 1, arguments.length);

      if (invokeAnyStaticFunctionNode == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        assert resolvedFuncArgCount >= 2
            : "Resolved function should be on Any.type, therefore, should have at least two self"
                + " arguments";
        // Prepend self=Any to the arguments and shift
        CallArgumentInfo[] newInvokeFuncSchema = new CallArgumentInfo[arguments.length + 1];
        newInvokeFuncSchema[0] = new CallArgumentInfo("self");
        assert arguments.length == invokeFuncSchema.length;
        System.arraycopy(invokeFuncSchema, 0, newInvokeFuncSchema, 1, invokeFuncSchema.length);

        assert argsWithPrependedSelf.length == newInvokeFuncSchema.length;
        invokeAnyStaticFunctionNode =
            insert(
                InvokeFunctionNode.build(
                    newInvokeFuncSchema,
                    DefaultsExecutionMode.EXECUTE,
                    ArgumentsExecutionMode.EXECUTE));
      }
      assert argsWithPrependedSelf.length == invokeAnyStaticFunctionNode.getSchema().length;
      assert Arrays.stream(argsWithPrependedSelf).allMatch(Objects::nonNull);
      return invokeAnyStaticFunctionNode.execute(function, frame, state, argsWithPrependedSelf);
    }
    assert arguments.length == invokeFunctionNode.getSchema().length;
    return invokeFunctionNode.execute(function, frame, state, arguments);
  }

  @Specialization
  Object doDataflowError(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      DataflowError self,
      Object[] arguments,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
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
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings) {
    Object selfWithoutWarnings;
    Warning[] arrOfWarnings;
    try {
      selfWithoutWarnings = warnings.removeWarnings(self);
      arrOfWarnings = warnings.getWarnings(self, this);
    } catch (UnsupportedMessageException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
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
                      thisArgumentPosition, false));
          childDispatch.setTailStatus(getTailStatus());
          childDispatch.setId(invokeFunctionNode.getId());
          notifyInserted(childDispatch);
        }
      } finally {
        lock.unlock();
      }
    }

    arguments[thisArgumentPosition] = selfWithoutWarnings;

    try {
      Object result = childDispatch.execute(frame, state, symbol, selfWithoutWarnings, arguments);
      return WithWarnings.appendTo(EnsoContext.get(this), result, arrOfWarnings);
    } catch (TailCallException e) {
      throw new TailCallException(e, arrOfWarnings);
    }
  }

  @ExplodeLoop
  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
        "!methods.hasType(self)",
        "!methods.hasSpecialDispatch(self)",
        "polyglotCallType.isInteropLibrary()",
      }, limit = "3")
  Object doPolyglot(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary methods,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode preResolveMethod,
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
      var r = argExecutors[i].executeThunk(frame, arguments[i + 1], state, TailStatus.NOT_TAIL);
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
          throw CompilerDirectives.shouldNotReachHere(e);
        }
      } else {
        args[i] = r;
      }
    }
    Object res = hostMethodCallNode.execute(polyglotCallType, symbol.getName(), self, args);
    if (anyWarnings) {
      anyWarningsProfile.enter();
      res = WithWarnings.appendTo(EnsoContext.get(this), res, accumulatedWarnings);
    }
    return res;
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop) == CONVERT_TO_BIG_INT"
      })
  Object doConvertNumber(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode,
      @Cached ToEnsoNumberNode toEnsoNumberNode) {
    try {
      var big = interop.asBigInteger(self);
      var ensoBig = toEnsoNumberNode.execute(big);
      arguments[0] = ensoBig;
      return execute(frame, state, symbol, ensoBig, arguments);
    } catch (UnsupportedMessageException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
    }
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
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
    try {
      var str = interop.asString(self);
      var text = Text.create(str);
      var ctx = EnsoContext.get(this);
      var textType = ctx.getBuiltins().text();
      var function = methodResolverNode.expectNonNull(text, textType, symbol);
      arguments[0] = text;
      return invokeFunctionNode.execute(function, frame, state, arguments);
    } catch (UnsupportedMessageException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
    }
  }

  @Specialization(
      guards = {
        "!warnings.hasWarnings(self)",
        "!types.hasType(self)",
        "!types.hasSpecialDispatch(self)",
        "getPolyglotCallType(self, symbol, interop, methodResolverNode) == CONVERT_TO_ARRAY",
      }, limit = "3")
  Object doConvertArray(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      }, limit = "3")
  Object doConvertHashMap(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary types,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode methodResolverNode) {
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
      @Shared("types") @CachedLibrary(limit = "10") TypesLibrary methods,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("warnings") @CachedLibrary(limit = "10") WarningsLibrary warnings,
      @Shared("methodResolverNode") @Cached MethodResolverNode resolverNode) {
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
