package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.instrument.execution.LocationFilter;
import org.enso.interpreter.instrument.execution.Timer;
import org.enso.interpreter.instrument.profiling.ExecutionTime;
import org.enso.interpreter.instrument.profiling.ProfilingInfo;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.runtime.type.Types;
import org.enso.pkg.QualifiedName;

import java.util.*;
import java.util.function.Consumer;

/** An instrument for getting values from AST-identified expressions. */
@TruffleInstrument.Registration(
    id = IdExecutionInstrument.INSTRUMENT_ID,
    services = IdExecutionInstrument.class)
public class IdExecutionInstrument extends TruffleInstrument {
  public static final String INSTRUMENT_ID = "id-value-extractor";

  private Timer timer;
  private Env env;

  /**
   * Initializes the instrument. Substitute for a constructor, called by the Truffle framework.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    this.timer = new Timer.Nanosecond();
    this.env = env;
  }

  /**
   * Override the default nanosecond timer with the specified {@code timer}.
   *
   * @param timer the timer to override with
   */
  public void overrideTimer(Timer timer) {
    this.timer = timer;
  }

  /** A class for notifications about functions being called in the course of execution. */
  public static class ExpressionCall {
    private final UUID expressionId;
    private final FunctionCallInstrumentationNode.FunctionCall call;

    /**
     * Creates an instance of this class.
     *
     * @param expressionId the expression id where function call was performed.
     * @param call the actual function call data.
     */
    public ExpressionCall(UUID expressionId, FunctionCallInstrumentationNode.FunctionCall call) {
      this.expressionId = expressionId;
      this.call = call;
    }

    /** @return the id of the node performing the function call. */
    public UUID getExpressionId() {
      return expressionId;
    }

    /** @return the function call metadata. */
    public FunctionCallInstrumentationNode.FunctionCall getCall() {
      return call;
    }
  }

  /** A class for notifications about identified expressions' values being computed. */
  public static class ExpressionValue {
    private final UUID expressionId;
    private final Object value;
    private final String type;
    private final String cachedType;
    private final FunctionCallInfo callInfo;
    private final FunctionCallInfo cachedCallInfo;
    private final ProfilingInfo[] profilingInfo;
    private final boolean wasCached;

    /**
     * Creates a new instance of this class.
     *
     * @param expressionId the id of the expression being computed.
     * @param value the value returned by computing the expression.
     * @param type the type of the returned value.
     * @param cachedType the cached type of the value.
     * @param callInfo the function call data.
     * @param cachedCallInfo the cached call data.
     * @param profilingInfo the profiling information associated with this node
     * @param wasCached whether or not the value was obtained from the cache
     */
    public ExpressionValue(
        UUID expressionId,
        Object value,
        String type,
        String cachedType,
        FunctionCallInfo callInfo,
        FunctionCallInfo cachedCallInfo,
        ProfilingInfo[] profilingInfo,
        boolean wasCached) {
      this.expressionId = expressionId;
      this.value = value;
      this.type = type;
      this.cachedType = cachedType;
      this.callInfo = callInfo;
      this.cachedCallInfo = cachedCallInfo;
      this.profilingInfo = profilingInfo;
      this.wasCached = wasCached;
    }

    @Override
    public String toString() {
      String profilingInfo = Arrays.toString(this.profilingInfo);
      return "ExpressionValue{"
          + "expressionId="
          + expressionId
          + ", value="
          + value
          + ", type='"
          + type
          + '\''
          + ", cachedType='"
          + cachedType
          + '\''
          + ", callInfo="
          + callInfo
          + ", cachedCallInfo="
          + cachedCallInfo
          + ", profilingInfo="
          + profilingInfo
          + ", wasCached="
          + wasCached
          + '}';
    }

    /** @return the id of the expression computed. */
    public UUID getExpressionId() {
      return expressionId;
    }

    /** @return the type of the returned value. */
    public String getType() {
      return type;
    }

    /** @return the cached type of the value. */
    public String getCachedType() {
      return cachedType;
    }

    /** @return the computed value of the expression. */
    public Object getValue() {
      return value;
    }

    /** @return the function call data. */
    public FunctionCallInfo getCallInfo() {
      return callInfo;
    }

    /** @return the function call data previously associated with the expression. */
    public FunctionCallInfo getCachedCallInfo() {
      return cachedCallInfo;
    }

    /** @return the profiling information associated with this expression */
    public ProfilingInfo[] getProfilingInfo() {
      return profilingInfo;
    }

    /** @return whether or not the expression result was obtained from the cache */
    public boolean wasCached() {
      return wasCached;
    }

    /** @return {@code true} when the type differs from the cached value. */
    public boolean isTypeChanged() {
      return !Objects.equals(type, cachedType);
    }

    /** @return {@code true} when the function call differs from the cached value. */
    public boolean isFunctionCallChanged() {
      return !Objects.equals(callInfo, cachedCallInfo);
    }
  }

  /** Information about the function call. */
  public static class FunctionCallInfo {

    private final QualifiedName moduleName;
    private final QualifiedName typeName;
    private final String functionName;

    /**
     * Creates a new instance of this class.
     *
     * @param call the function call.
     */
    public FunctionCallInfo(FunctionCallInstrumentationNode.FunctionCall call) {
      RootNode rootNode = call.getFunction().getCallTarget().getRootNode();
      if (rootNode instanceof MethodRootNode) {
        MethodRootNode methodNode = (MethodRootNode) rootNode;
        moduleName = methodNode.getModuleScope().getModule().getName();
        typeName = methodNode.getAtomConstructor().getQualifiedName();
        functionName = methodNode.getMethodName();
      } else if (rootNode instanceof EnsoRootNode) {
        moduleName = ((EnsoRootNode) rootNode).getModuleScope().getModule().getName();
        typeName = null;
        functionName = rootNode.getName();
      } else {
        moduleName = null;
        typeName = null;
        functionName = rootNode.getName();
      }
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      FunctionCallInfo that = (FunctionCallInfo) o;
      return Objects.equals(moduleName, that.moduleName)
          && Objects.equals(typeName, that.typeName)
          && Objects.equals(functionName, that.functionName);
    }

    @Override
    public int hashCode() {
      return Objects.hash(moduleName, typeName, functionName);
    }

    @Override
    public String toString() {
      return moduleName + "::" + typeName + "::" + functionName;
    }

    /** @return the name of the module this function was defined in, or null if not available. */
    public QualifiedName getModuleName() {
      return moduleName;
    }

    /** @return the name of the type this method was defined for, or null if not a method. */
    public QualifiedName getTypeName() {
      return typeName;
    }

    /** @return the name of this function. */
    public String getFunctionName() {
      return functionName;
    }
  }

  /** The listener class used by this instrument. */
  private static class IdExecutionEventListener implements ExecutionEventListener {
    private final CallTarget entryCallTarget;
    private final Consumer<ExpressionCall> functionCallCallback;
    private final Consumer<ExpressionValue> onComputedCallback;
    private final Consumer<ExpressionValue> onCachedCallback;
    private final Consumer<Exception> onExceptionalCallback;
    private final RuntimeCache cache;
    private final MethodCallsCache callsCache;
    private final UpdatesSynchronizationState syncState;
    private final UUID nextExecutionItem;
    private final Map<UUID, FunctionCallInfo> calls = new HashMap<>();
    private final Timer timer;
    private long nanoTimeElapsed = 0;

    /**
     * Creates a new listener.
     *
     * @param entryCallTarget the call target being observed.
     * @param cache the precomputed expression values.
     * @param methodCallsCache the storage tracking the executed method calls.
     * @param syncState the synchronization state of runtime updates.
     * @param nextExecutionItem the next item scheduled for execution.
     * @param functionCallCallback the consumer of function call events.
     * @param onComputedCallback the consumer of the computed value events.
     * @param onCachedCallback the consumer of the cached value events.
     * @param onExceptionalCallback the consumer of the exceptional events.
     * @param timer the timer for timing execution
     */
    public IdExecutionEventListener(
        CallTarget entryCallTarget,
        RuntimeCache cache,
        MethodCallsCache methodCallsCache,
        UpdatesSynchronizationState syncState,
        UUID nextExecutionItem, // The expression ID
        Consumer<ExpressionCall> functionCallCallback,
        Consumer<ExpressionValue> onComputedCallback,
        Consumer<ExpressionValue> onCachedCallback,
        Consumer<Exception> onExceptionalCallback,
        Timer timer) {
      this.entryCallTarget = entryCallTarget;
      this.cache = cache;
      this.callsCache = methodCallsCache;
      this.syncState = syncState;
      this.nextExecutionItem = nextExecutionItem;
      this.functionCallCallback = functionCallCallback;
      this.onComputedCallback = onComputedCallback;
      this.onCachedCallback = onCachedCallback;
      this.onExceptionalCallback = onExceptionalCallback;
      this.timer = timer;
    }

    @Override
    public Object onUnwind(EventContext context, VirtualFrame frame, Object info) {
      return info;
    }

    @Override
    public void onEnter(EventContext context, VirtualFrame frame) {
      if (!isTopFrame(entryCallTarget)) {
        return;
      }
      UUID nodeId = getNodeId(context.getInstrumentedNode());

      // Add a flag to say it was cached.
      // An array of `ProfilingInfo` in the value update.

      Object result = cache.get(nodeId);
      // When executing the call stack we need to capture the FunctionCall of the next (top) stack
      // item in the `functionCallCallback`. We allow to execute the cached `stackTop` value to be
      // able to continue the stack execution, and unwind later from the `onReturnValue` callback.
      if (result != null && !nodeId.equals(nextExecutionItem)) {
        onCachedCallback.accept(
            new ExpressionValue(
                nodeId,
                result,
                cache.getType(nodeId),
                Types.getName(result),
                calls.get(nodeId),
                cache.getCall(nodeId),
                new ProfilingInfo[] {ExecutionTime.empty()},
                true));
        throw context.createUnwind(result);
      }

      nanoTimeElapsed = timer.getTime();
    }

    /**
     * Triggered when a node (either a function call sentry or an identified expression) finishes
     * execution.
     *
     * @param context the event context.
     * @param frame the current execution frame.
     * @param result the result of executing the node this method was triggered for.
     */
    @Override
    public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
      nanoTimeElapsed = timer.getTime() - nanoTimeElapsed;
      if (!isTopFrame(entryCallTarget)) {
        return;
      }
      Node node = context.getInstrumentedNode();

      if (node instanceof FunctionCallInstrumentationNode
          && result instanceof FunctionCallInstrumentationNode.FunctionCall) {
        UUID nodeId = ((FunctionCallInstrumentationNode) node).getId();
        calls.put(
            nodeId, new FunctionCallInfo((FunctionCallInstrumentationNode.FunctionCall) result));
        functionCallCallback.accept(
            new ExpressionCall(nodeId, (FunctionCallInstrumentationNode.FunctionCall) result));
        // Return cached value after capturing the enterable function call in `functionCallCallback`
        Object cachedResult = cache.get(nodeId);
        if (cachedResult != null) {
          throw context.createUnwind(cachedResult);
        }
        callsCache.setExecuted(nodeId);
      } else if (node instanceof ExpressionNode) {
        boolean isPanic = result instanceof PanicSentinel;
        UUID nodeId = ((ExpressionNode) node).getId();
        String resultType = Types.getName(result);

        String cachedType = cache.getType(nodeId);
        FunctionCallInfo call = calls.get(nodeId);
        FunctionCallInfo cachedCall = cache.getCall(nodeId);
        ProfilingInfo[] profilingInfo = new ProfilingInfo[] {new ExecutionTime(nanoTimeElapsed)};

        ExpressionValue expressionValue =
            new ExpressionValue(
                nodeId, result, resultType, cachedType, call, cachedCall, profilingInfo, false);
        if (expressionValue.isTypeChanged() || expressionValue.isFunctionCallChanged()) {
          syncState.setExpressionUnsync(nodeId);
        }
        syncState.setVisualisationUnsync(nodeId);

        // Panics are not cached because a panic can be fixed by changing seemingly unrelated code,
        // like imports, and the invalidation mechanism can not always track those changes and
        // appropriately invalidate all dependent expressions.
        if (!isPanic) {
          cache.offer(nodeId, result);
        }
        cache.putType(nodeId, resultType);
        cache.putCall(nodeId, call);

        onComputedCallback.accept(expressionValue);
        if (isPanic) {
          throw context.createUnwind(result);
        }
      }
    }

    @Override
    public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
      if (exception instanceof TailCallException) {
        try {
          TailCallException tailCallException = (TailCallException) exception;
          FunctionCallInstrumentationNode.FunctionCall functionCall =
              new FunctionCallInstrumentationNode.FunctionCall(
                  tailCallException.getFunction(),
                  tailCallException.getState(),
                  tailCallException.getArguments());
          Object result = InteropLibrary.getFactory().getUncached().execute(functionCall);
          onReturnValue(context, frame, result);
        } catch (InteropException e) {
          onExceptionalCallback.accept(e);
        }
      } else if (exception instanceof PanicException) {
        PanicException panicException = (PanicException) exception;
        onReturnValue(
            context, frame, new PanicSentinel(panicException, context.getInstrumentedNode()));
      } else if (exception instanceof PanicSentinel) {
        onReturnValue(context, frame, exception);
      }
    }

    /**
     * Checks if we're not inside a recursive call, i.e. the {@link #entryCallTarget} only appears
     * in the stack trace once.
     *
     * @return {@code true} if it's not a recursive call, {@code false} otherwise.
     */
    private boolean isTopFrame(CallTarget entryCallTarget) {
      Object result =
          Truffle.getRuntime()
              .iterateFrames(
                  new FrameInstanceVisitor<Object>() {
                    boolean seenFirst = false;

                    @Override
                    public Object visitFrame(FrameInstance frameInstance) {
                      CallTarget ct = frameInstance.getCallTarget();
                      if (ct != entryCallTarget) {
                        return null;
                      }
                      if (seenFirst) {
                        return new Object();
                      } else {
                        seenFirst = true;
                        return null;
                      }
                    }
                  });
      return result == null;
    }

    private UUID getNodeId(Node node) {
      if (node instanceof ExpressionNode) {
        return ((ExpressionNode) node).getId();
      }
      if (node instanceof FunctionCallInstrumentationNode) {
        return ((FunctionCallInstrumentationNode) node).getId();
      }
      return null;
    }
  }

  /**
   * Attach a new listener to observe identified nodes within given function.
   *
   * @param entryCallTarget the call target being observed.
   * @param locationFilter the location filter.
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed method calls.
   * @param syncState the synchronization state of runtime updates.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param functionCallCallback the consumer of function call events.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param onExceptionalCallback the consumer of the exceptional events.
   * @return a reference to the attached event listener.
   */
  public EventBinding<ExecutionEventListener> bind(
      CallTarget entryCallTarget,
      LocationFilter locationFilter,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UpdatesSynchronizationState syncState,
      UUID nextExecutionItem,
      Consumer<IdExecutionInstrument.ExpressionCall> functionCallCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onComputedCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onCachedCallback,
      Consumer<Exception> onExceptionalCallback) {
    SourceSectionFilter filter =
        SourceSectionFilter.newBuilder()
            .tagIs(StandardTags.ExpressionTag.class, StandardTags.CallTag.class)
            .tagIs(IdentifiedTag.class)
            .sourceSectionEquals(locationFilter.getSections())
            .build();

    return env.getInstrumenter()
        .attachExecutionEventListener(
            filter,
            new IdExecutionEventListener(
                entryCallTarget,
                cache,
                methodCallsCache,
                syncState,
                nextExecutionItem,
                functionCallCallback,
                onComputedCallback,
                onCachedCallback,
                onExceptionalCallback,
                timer));
  }
}
