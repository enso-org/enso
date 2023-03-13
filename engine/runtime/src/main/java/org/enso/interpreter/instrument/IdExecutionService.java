package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.Arrays;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Consumer;
import org.enso.interpreter.instrument.execution.Timer;
import org.enso.interpreter.instrument.profiling.ProfilingInfo;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.node.expression.atom.QualifiedAccessorNode;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.logger.masking.MaskedString;
import org.enso.pkg.QualifiedName;

public interface IdExecutionService {
  public static final String INSTRUMENT_ID = "id-value-extractor";

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
  EventBinding<ExecutionEventNodeFactory> bind(
      Module module,
      CallTarget entryCallTarget,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UpdatesSynchronizationState syncState,
      Timer timer,
      UUID nextExecutionItem,
      Consumer<ExpressionCall> functionCallCallback,
      Consumer<ExpressionValue> onComputedCallback,
      Consumer<ExpressionValue> onCachedCallback,
      Consumer<Exception> onExceptionalCallback);

  /** A class for notifications about functions being called in the course of execution. */
  class ExpressionCall {
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
  class ExpressionValue {
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
          + (value == null ? "null" : new MaskedString(value.toString()).applyMasking())
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
  class FunctionCallInfo {

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
      if (rootNode instanceof MethodRootNode methodNode) {
        moduleName = methodNode.getModuleScope().getModule().getName();
        typeName = methodNode.getType().getQualifiedName();
        functionName = methodNode.getMethodName();
      } else if (rootNode instanceof QualifiedAccessorNode qualifiedAccessor) {
        AtomConstructor atomConstructor = qualifiedAccessor.getAtomConstructor();
        moduleName = atomConstructor.getDefinitionScope().getModule().getName();
        typeName = atomConstructor.getType().getQualifiedName();
        functionName = atomConstructor.getDisplayName();
      } else if (rootNode instanceof EnsoRootNode ensoRootNode) {
        moduleName = ensoRootNode.getModuleScope().getModule().getName();
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
}
