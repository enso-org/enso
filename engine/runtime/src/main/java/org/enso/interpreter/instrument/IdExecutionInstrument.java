package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.runtime.type.Types;
import org.enso.pkg.QualifiedName;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Consumer;

/** An instrument for getting values from AST-identified expressions. */
@TruffleInstrument.Registration(
    id = IdExecutionInstrument.INSTRUMENT_ID,
    services = IdExecutionInstrument.class)
public class IdExecutionInstrument extends TruffleInstrument {
  public static final String INSTRUMENT_ID = "id-value-extractor";

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

    /**
     * Creates a new instance of this class.
     *
     * @param expressionId the id of the expression being computed.
     * @param value the value returned by computing the expression.
     * @param type the type of the returned value.
     * @param cachedType the cached type of the value.
     * @param callInfo the function call data.
     */
    public ExpressionValue(
        UUID expressionId,
        Object value,
        String type,
        String cachedType,
        FunctionCallInfo callInfo,
        FunctionCallInfo cachedCallInfo) {
      this.expressionId = expressionId;
      this.value = value;
      this.type = type;
      this.cachedType = cachedType;
      this.callInfo = callInfo;
      this.cachedCallInfo = cachedCallInfo;
    }

    @Override
    public String toString() {
      return "ExpressionValue{" +
          "expressionId=" + expressionId +
          ", value=" + value +
          ", type='" + type + '\'' +
          ", cachedType='" + cachedType + '\'' +
          ", callInfo=" + callInfo +
          ", cachedCallInfo=" + cachedCallInfo +
          '}';
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
        functionName = rootNode.getQualifiedName();
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
      return Objects.equals(moduleName, that.moduleName) &&
          Objects.equals(typeName, that.typeName) &&
          functionName.equals(that.functionName);
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
    private final RuntimeCache cache;
    private final MethodCallsCache callsCache;
    private final UUID nextExecutionItem;
    private final Map<UUID, FunctionCallInfo> calls = new HashMap<>();

    /**
     * Creates a new listener.
     *
     * @param entryCallTarget the call target being observed.
     * @param cache the precomputed expression values.
     * @param methodCallsCache the storage tracking the executed method calls.
     * @param nextExecutionItem the next item scheduled for execution.
     * @param functionCallCallback the consumer of function call events.
     * @param onComputedCallback the consumer of the computed value events.
     * @param onCachedCallback the consumer of the cached value events.
     */
    public IdExecutionEventListener(
        CallTarget entryCallTarget,
        RuntimeCache cache,
        MethodCallsCache methodCallsCache,
        UUID nextExecutionItem,
        Consumer<ExpressionCall> functionCallCallback,
        Consumer<ExpressionValue> onComputedCallback,
        Consumer<ExpressionValue> onCachedCallback) {
      this.entryCallTarget = entryCallTarget;
      this.cache = cache;
      this.callsCache = methodCallsCache;
      this.nextExecutionItem = nextExecutionItem;
      this.functionCallCallback = functionCallCallback;
      this.onComputedCallback = onComputedCallback;
      this.onCachedCallback = onCachedCallback;
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

      Node node = context.getInstrumentedNode();

      UUID nodeId = null;
      if (node instanceof ExpressionNode) {
        nodeId = ((ExpressionNode) node).getId();
      } else if (node instanceof FunctionCallInstrumentationNode) {
        nodeId = ((FunctionCallInstrumentationNode) node).getId();
      }

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
                cache.getCall(nodeId)));
        throw context.createUnwind(result);
      }
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
        UUID nodeId = ((ExpressionNode) node).getId();
        String resultType = Types.getName(result);
        cache.offer(nodeId, result);
        String cachedType = cache.putType(nodeId, resultType);
        FunctionCallInfo call = calls.get(nodeId);
        FunctionCallInfo cachedCall = cache.putCall(nodeId, call);
        onComputedCallback.accept(
            new ExpressionValue(nodeId, result, resultType, cachedType, call, cachedCall));
      }
    }

    @Override
    public void onReturnExceptional(
        EventContext context, VirtualFrame frame, Throwable exception) {}

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
  }

  /**
   * Attach a new listener to observe identified nodes within given function.
   *
   * @param entryCallTarget the call target being observed.
   * @param funSourceStart the source start of the observed range of ids.
   * @param funSourceLength the length of the observed source range.
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed method calls.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param functionCallCallback the consumer of function call events.
   * @return a reference to the attached event listener.
   */
  public EventBinding<ExecutionEventListener> bind(
      CallTarget entryCallTarget,
      int funSourceStart,
      int funSourceLength,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UUID nextExecutionItem,
      Consumer<ExpressionValue> onComputedCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onCachedCallback,
      Consumer<ExpressionCall> functionCallCallback) {
    SourceSectionFilter filter =
        SourceSectionFilter.newBuilder()
            .tagIs(StandardTags.ExpressionTag.class, StandardTags.CallTag.class)
            .tagIs(IdentifiedTag.class)
            .indexIn(funSourceStart, funSourceLength)
            .build();

    EventBinding<ExecutionEventListener> binding =
        env.getInstrumenter()
            .attachExecutionEventListener(
                filter,
                new IdExecutionEventListener(
                    entryCallTarget,
                    cache,
                    methodCallsCache,
                    nextExecutionItem,
                    functionCallCallback,
                    onComputedCallback,
                    onCachedCallback));
    return binding;
  }
}
