package org.enso.interpreter.service;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

import org.enso.interpreter.instrument.IdExecutionService;
import org.enso.interpreter.instrument.IdExecutionService.ExpressionCall;
import org.enso.interpreter.instrument.IdExecutionService.ExpressionValue;
import org.enso.interpreter.instrument.IdExecutionService.FunctionCallInfo;
import org.enso.interpreter.instrument.MethodCallsCache;
import org.enso.interpreter.instrument.RuntimeCache;
import org.enso.interpreter.instrument.UpdatesSynchronizationState;
import org.enso.interpreter.instrument.profiling.ExecutionTime;
import org.enso.interpreter.instrument.profiling.ProfilingInfo;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.type.Constants;

import com.oracle.truffle.api.CompilerDirectives;

final class ExecutionCallbacks implements IdExecutionService.Callbacks {

  private final UUID nextExecutionItem;
  private final RuntimeCache cache;
  private final MethodCallsCache methodCallsCache;
  private final UpdatesSynchronizationState syncState;
  private final Map<UUID, FunctionCallInfo> calls = new HashMap<>();
  private final Consumer<ExpressionValue> onCachedCallback;
  private final Consumer<ExpressionValue> onComputedCallback;
  private final Consumer<ExpressionCall> functionCallCallback;
  private final Consumer<Exception> onExceptionalCallback;

  /** Creates callbacks instance.
   *
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed updateCachedResult calls.
   * @param syncState the synchronization state of runtime updates.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param functionCallCallback the consumer of function call events.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param onExceptionalCallback the consumer of the exceptional events.
   */
  ExecutionCallbacks(
          UUID nextExecutionItem,
          RuntimeCache cache, MethodCallsCache methodCallsCache, UpdatesSynchronizationState syncState,
          Consumer<ExpressionValue> onCachedCallback, Consumer<ExpressionValue> onComputedCallback,
          Consumer<ExpressionCall> functionCallCallback, Consumer<Exception> onExceptionalCallback
  ) {
    this.nextExecutionItem = nextExecutionItem;
    this.cache = cache;
    this.methodCallsCache = methodCallsCache;
    this.syncState = syncState;
    this.onCachedCallback = onCachedCallback;
    this.onComputedCallback = onComputedCallback;
    this.functionCallCallback = functionCallCallback;
    this.onExceptionalCallback = onExceptionalCallback;
  }

  @CompilerDirectives.TruffleBoundary
  public final Object findCachedResult(UUID nodeId) {
    // Add a flag to say it was cached.
    // An array of `ProfilingInfo` in the value update.
    Object result = cache.get(nodeId);
    // When executing the call stack we need to capture the FunctionCall of the next (top) stack
    // item in the `functionCallCallback`. We allow to execute the cached `stackTop` value to be
    // able to continue the stack execution, and unwind later from the `onReturnValue` callback.
    if (result != null && !nodeId.equals(nextExecutionItem)) {
      var value = new ExpressionValue(
              nodeId,
              result,
              cache.getType(nodeId),
              typeOf(result),
              calls.get(nodeId),
              cache.getCall(nodeId),
              new ProfilingInfo[]{ExecutionTime.empty()},
              true
      );
      onCachedCallback.accept(value);
      return result;
    }
    return null;
  }

  @CompilerDirectives.TruffleBoundary
  public final void updateCachedResult(UUID nodeId, Object result, boolean isPanic, long nanoTimeElapsed) {
    String resultType = typeOf(result);
    String cachedType = cache.getType(nodeId);
    FunctionCallInfo call = functionCallInfoById(nodeId);
    FunctionCallInfo cachedCall = cache.getCall(nodeId);
    ProfilingInfo[] profilingInfo = new ProfilingInfo[]{new ExecutionTime(nanoTimeElapsed)};

    ExpressionValue expressionValue
            = new ExpressionValue(nodeId, result, resultType, cachedType, call, cachedCall, profilingInfo, false);
    syncState.setExpressionUnsync(nodeId);
    syncState.setVisualizationUnsync(nodeId);

    // Panics are not cached because a panic can be fixed by changing seemingly unrelated code,
    // like imports, and the invalidation mechanism can not always track those changes and
    // appropriately invalidate all dependent expressions.
    if (!isPanic) {
      cache.offer(nodeId, result);
      cache.putCall(nodeId, call);
    }
    cache.putType(nodeId, resultType);

    passExpressionValueToCallback(expressionValue);
    if (isPanic) {
      // We mark the node as executed so that it is not reported as not executed call after the
      // program execution is complete. If we clear the call from the cache instead, it will mess
      // up the `typeChanged` field of the expression update.
      methodCallsCache.setExecuted(nodeId);
    }
  }

  @CompilerDirectives.TruffleBoundary
  public final Object onFunctionReturn(UUID nodeId, FunctionCallInstrumentationNode.FunctionCall result) {
    calls.put(nodeId, FunctionCallInfo.fromFunctionCall(result));
    functionCallCallback.accept(new ExpressionCall(nodeId, result));
    // Return cached value after capturing the enterable function call in `functionCallCallback`
    Object cachedResult = cache.get(nodeId);
    if (cachedResult != null) {
      return cachedResult;
    }
    methodCallsCache.setExecuted(nodeId);
    return null;
  }

  @CompilerDirectives.TruffleBoundary
  @Override
  public final void onExceptionalCallback(Exception e) {
    onExceptionalCallback.accept(e);
  }

  @CompilerDirectives.TruffleBoundary
  private void passExpressionValueToCallback(ExpressionValue expressionValue) {
    onComputedCallback.accept(expressionValue);
  }

  @CompilerDirectives.TruffleBoundary
  private FunctionCallInfo functionCallInfoById(UUID nodeId) {
    return calls.get(nodeId);
  }

  private String typeOf(Object value) {
    String resultType;
    if (value instanceof UnresolvedSymbol) {
      resultType = Constants.UNRESOLVED_SYMBOL;
    } else {
      var typeOfNode = TypeOfNode.getUncached();
      Object typeResult = typeOfNode.execute(value);
      if (typeResult instanceof Type t) {
        resultType = t.getQualifiedName().toString();
      } else {
        resultType = null;
      }
    }
    return resultType;
  }
}
