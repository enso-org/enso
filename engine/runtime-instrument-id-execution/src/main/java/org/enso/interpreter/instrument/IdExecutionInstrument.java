package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.enso.interpreter.instrument.execution.Timer;
import org.enso.interpreter.instrument.profiling.ExecutionTime;
import org.enso.interpreter.instrument.profiling.ProfilingInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.runtime.type.Types;
import org.enso.interpreter.runtime.Module;

import java.util.function.Consumer;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;

/** An instrument for getting values from AST-identified expressions. */
@TruffleInstrument.Registration(
    id = IdExecutionService.INSTRUMENT_ID,
    services = IdExecutionService.class)
public class IdExecutionInstrument extends TruffleInstrument implements IdExecutionService {
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
  @Override
  public void overrideTimer(Timer timer) {
    this.timer = timer;
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
      onEnterImpl(context);
    }

    @CompilerDirectives.TruffleBoundary
    private void onEnterImpl(EventContext context) {
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
        onFunctionReturn(nodeId, result, context);
      } else if (node instanceof ExpressionNode) {
        onExpressionReturn(result, node, context);
      }
    }

    @Override
    public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
      if (exception instanceof TailCallException) {
        onTailCallReturn(exception, context);
      } else if (exception instanceof PanicException) {
        PanicException panicException = (PanicException) exception;
        onReturnValue(
            context, frame, new PanicSentinel(panicException, context.getInstrumentedNode()));
      } else if (exception instanceof PanicSentinel) {
        onReturnValue(context, frame, exception);
      }
    }

    @CompilerDirectives.TruffleBoundary
    private void onExpressionReturn(Object result, Node node, EventContext context) throws ThreadDeath {
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
        syncState.setExpressionUnsync(nodeId);
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


    @CompilerDirectives.TruffleBoundary
    private void onFunctionReturn(UUID nodeId, Object result, EventContext context) throws ThreadDeath {
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
    }

    @CompilerDirectives.TruffleBoundary
    private void onTailCallReturn(Throwable exception, EventContext context) {
        try {
            TailCallException tailCallException = (TailCallException) exception;
            FunctionCallInstrumentationNode.FunctionCall functionCall =
                    new FunctionCallInstrumentationNode.FunctionCall(
                            tailCallException.getFunction(),
                            tailCallException.getState(),
                            tailCallException.getArguments());
            Object result = InteropLibrary.getFactory().getUncached().execute(functionCall);
            onReturnValue(context, null, result);
        } catch (InteropException e) {
            onExceptionalCallback.accept(e);
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
   * @param module module that contains the code
   * @param entryCallTarget the call target being observed.
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
  @Override
  public EventBinding<ExecutionEventListener> bind(
      Module module,
      CallTarget entryCallTarget,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UpdatesSynchronizationState syncState,
      UUID nextExecutionItem,
      Consumer<IdExecutionInstrument.ExpressionCall> functionCallCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onComputedCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onCachedCallback,
      Consumer<Exception> onExceptionalCallback) {
    var builder = SourceSectionFilter.newBuilder()
          .tagIs(StandardTags.ExpressionTag.class, StandardTags.CallTag.class)
          .tagIs(IdentifiedTag.class)
          .tagIsNot(AvoidIdInstrumentationTag.class)
          .sourceIs(module::isModuleSource);

    if (entryCallTarget instanceof RootCallTarget r && r.getRootNode() instanceof ClosureRootNode c && c.getSourceSection() != null) {
      final int firstFunctionLine = c.getSourceSection().getStartLine();
      final int afterFunctionLine = c.getSourceSection().getEndLine() + 1;
      builder.lineIn(SourceSectionFilter.IndexRange.between(firstFunctionLine, afterFunctionLine));
    }
    SourceSectionFilter filter = builder.build();

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
