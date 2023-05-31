package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.exception.AbstractTruffleException;
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
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.runtime.type.Constants;
import org.enso.interpreter.runtime.Module;

import java.util.function.Consumer;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;

/** An instrument for getting values from AST-identified expressions. */
@TruffleInstrument.Registration(
    id = IdExecutionService.INSTRUMENT_ID,
    services = IdExecutionService.class)
public class IdExecutionInstrument extends TruffleInstrument implements IdExecutionService {

  private Env env;

  /**
   * Initializes the instrument. Substitute for a constructor, called by the Truffle framework.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    this.env = env;
  }

  /** Factory for creating new id event nodes **/
  private static class IdEventNodeFactory implements ExecutionEventNodeFactory {

      private final CallTarget entryCallTarget;
      private final Consumer<ExpressionCall> functionCallCallback;
      private final Consumer<ExpressionValue> onComputedCallback;
      private final Consumer<ExpressionValue> onCachedCallback;
      private final Consumer<Exception> onExceptionalCallback;
      private final RuntimeCache cache;
      private final MethodCallsCache methodCallsCache;
      private final UpdatesSynchronizationState syncState;
      private final UUID nextExecutionItem;
      private final Map<UUID, FunctionCallInfo> calls = new HashMap<>();
      private final Timer timer;

      /**
       * Creates a new event node factory.
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
      public IdEventNodeFactory(
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
          this.methodCallsCache = methodCallsCache;
          this.syncState = syncState;
          this.nextExecutionItem = nextExecutionItem;
          this.functionCallCallback = functionCallCallback;
          this.onComputedCallback = onComputedCallback;
          this.onCachedCallback = onCachedCallback;
          this.onExceptionalCallback = onExceptionalCallback;
          this.timer = timer;
      }

      @Override
      public ExecutionEventNode create(EventContext context) {
          return new IdExecutionEventNode(context, entryCallTarget, cache, methodCallsCache, syncState,
                  nextExecutionItem, calls, functionCallCallback, onComputedCallback, onCachedCallback, onExceptionalCallback, timer);
      }
  }

  /** The execution event node class used by this instrument. */
  private static class IdExecutionEventNode extends ExecutionEventNode {
    private final EventContext context;
    private final CallTarget entryCallTarget;
    private final Consumer<ExpressionCall> functionCallCallback;
    private final Consumer<ExpressionValue> onComputedCallback;
    private final Consumer<ExpressionValue> onCachedCallback;
    private final Consumer<Exception> onExceptionalCallback;
    private final RuntimeCache cache;
    private final MethodCallsCache callsCache;
    private final UpdatesSynchronizationState syncState;
    private final UUID nextExecutionItem;
    private final Map<UUID, FunctionCallInfo> calls;
    private final Timer timer;
    private long nanoTimeElapsed = 0;
    private @Child TypeOfNode typeOfNode = TypeOfNode.build();

    /**
     * Creates a new event node.
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
    public IdExecutionEventNode(
        EventContext context,
        CallTarget entryCallTarget,
        RuntimeCache cache,
        MethodCallsCache methodCallsCache,
        UpdatesSynchronizationState syncState,
        UUID nextExecutionItem, // The expression ID
        Map<UUID, FunctionCallInfo> calls,
        Consumer<ExpressionCall> functionCallCallback,
        Consumer<ExpressionValue> onComputedCallback,
        Consumer<ExpressionValue> onCachedCallback,
        Consumer<Exception> onExceptionalCallback,
        Timer timer) {
      this.context = context;
      this.entryCallTarget = entryCallTarget;
      this.cache = cache;
      this.calls = calls;
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
    public Object onUnwind(VirtualFrame frame, Object info) {
      return info;
    }

    @Override
    public void onEnter(VirtualFrame frame) {
      if (!isTopFrame(entryCallTarget)) {
        return;
      }
      onEnterImpl();
    }

    @CompilerDirectives.TruffleBoundary
    private void onEnterImpl() {
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
                typeOf(result),
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
     * @param frame the current execution frame.
     * @param result the result of executing the node this method was triggered for.
     */
    @Override
    public void onReturnValue(VirtualFrame frame, Object result) {
      nanoTimeElapsed = timer.getTime() - nanoTimeElapsed;
      if (!isTopFrame(entryCallTarget)) {
        return;
      }
      Node node = context.getInstrumentedNode();

      if (node instanceof FunctionCallInstrumentationNode
          && result instanceof FunctionCallInstrumentationNode.FunctionCall functionCall) {
        UUID nodeId = ((FunctionCallInstrumentationNode) node).getId();
        onFunctionReturn(nodeId, functionCall, context);
      } else if (node instanceof ExpressionNode) {
        onExpressionReturn(result, node, context);
      }
    }

    @Override
    public void onReturnExceptional(VirtualFrame frame, Throwable exception) {
      if (exception instanceof TailCallException) {
        onTailCallReturn(exception, Function.ArgumentsHelper.getState(frame.getArguments()));
      } else if (exception instanceof PanicException) {
        PanicException panicException = (PanicException) exception;
        onReturnValue(frame, new PanicSentinel(panicException, context.getInstrumentedNode()));
      } else if (exception instanceof AbstractTruffleException) {
        onReturnValue(frame, exception);
      }
    }

    private void onExpressionReturn(Object result, Node node, EventContext context) throws ThreadDeath {
      boolean isPanic = result instanceof AbstractTruffleException;
      UUID nodeId = ((ExpressionNode) node).getId();

      String resultType = typeOf(result);
      String cachedType = cache.getType(nodeId);
      FunctionCallInfo call = functionCallInfoById(nodeId);
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

      passExpressionValueToCallback(expressionValue);
      if (isPanic) {
        throw context.createUnwind(result);
      }
    }

    private String typeOf(Object value) {
      String resultType;
      if (value instanceof UnresolvedSymbol) {
          resultType = Constants.UNRESOLVED_SYMBOL;
      } else {
          Object typeResult = typeOfNode.execute(value);
          if (typeResult instanceof Type t) {
              resultType = t.getQualifiedName().toString();
          } else {
              resultType = null;
          }
      }
      return resultType;
    }

    @CompilerDirectives.TruffleBoundary
    private void passExpressionValueToCallback(ExpressionValue expressionValue) {
        onComputedCallback.accept(expressionValue);
    }

    @CompilerDirectives.TruffleBoundary
    private FunctionCallInfo functionCallInfoById(UUID nodeId) {
        return calls.get(nodeId);
    }

    @CompilerDirectives.TruffleBoundary
    private void onFunctionReturn(UUID nodeId, FunctionCallInstrumentationNode.FunctionCall result, EventContext context) throws ThreadDeath {
        calls.put(nodeId, new FunctionCallInfo(result));
        functionCallCallback.accept(new ExpressionCall(nodeId, result));
        // Return cached value after capturing the enterable function call in `functionCallCallback`
        Object cachedResult = cache.get(nodeId);
        if (cachedResult != null) {
            throw context.createUnwind(cachedResult);
        }
        callsCache.setExecuted(nodeId);
    }

    @CompilerDirectives.TruffleBoundary
    private void onTailCallReturn(Throwable exception, State state) {
        try {
            TailCallException tailCallException = (TailCallException) exception;
            FunctionCallInstrumentationNode.FunctionCall functionCall =
                    new FunctionCallInstrumentationNode.FunctionCall(
                        tailCallException.getFunction(),
                        state,
                        tailCallException.getArguments());
            Object result = InteropLibrary.getFactory().getUncached().execute(functionCall);
            onReturnValue(null, result);
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
   * Attach a new event node factory to observe identified nodes within given function.
   *
   * @param module module that contains the code
   * @param entryCallTarget the call target being observed.
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed method calls.
   * @param syncState the synchronization state of runtime updates.
   * @param timer the execution timer.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param functionCallCallback the consumer of function call events.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param onExceptionalCallback the consumer of the exceptional events.
   * @return a reference to the attached event node factory.
   */
  @Override
  public EventBinding<ExecutionEventNodeFactory> bind(
      Module module,
      CallTarget entryCallTarget,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UpdatesSynchronizationState syncState,
      Timer timer,
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
        .attachExecutionEventFactory(
            filter,
            new IdEventNodeFactory(
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
