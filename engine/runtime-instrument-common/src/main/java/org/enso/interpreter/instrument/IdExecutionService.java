package org.enso.interpreter.instrument;

import java.util.Arrays;
import java.util.Objects;
import java.util.UUID;

import org.enso.interpreter.instrument.profiling.ProfilingInfo;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.node.expression.atom.QualifiedAccessorNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.Type;
import org.enso.logger.masking.MaskedString;
import org.enso.pkg.QualifiedName;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.nodes.RootNode;

public interface IdExecutionService {
  String INSTRUMENT_ID = "id-value-extractor";

  public interface Callbacks {
    /** Finds out previously computed result for given id. If
     * a result is returned, then the execution of given node is skipped
     * and the value is returned back.
     *
     * @param nodeId identification of the node to be computed
     * @return {@code null} should the execution of the node be performed;
     *   any other value to skip the execution and return the value as a
     *   result.
     */
    Object findCachedResult(UUID nodeId);

    /** Notifies when an execution of a node is over.
     * @param nodeId identification of the node to be computed
     * @param result the just computed result
     * @param isPanic was the result a panic?
     * @param nanoElapsedTime how long it took to compute the result?
     */
    void updateCachedResult(UUID nodeId, Object result, boolean isPanic, long nanoElapsedTime);

    /** Notification when a returned value is a function.
     *
     * @param nodeId identification of the node to be computed
     * @param result info about function call
     * @return {@code null} should the execution of the node be performed;
     *   any other value to skip the execution and return the value as a
     *   result.
     */
    Object onFunctionReturn(UUID nodeId, FunctionCallInstrumentationNode.FunctionCall result);

    /** Notification on an exception.
     * @param e the reported exception
     */
    void onExceptionalCallback(Exception e);
  }


  /**
   * Attach a new event node factory to observe identified nodes within given function.
   *
   * @param module module that contains the code
   * @param entryCallTarget the call target being observed.
   * @param callbacks the interface to receive notifications
   * @param timer the execution timer.
   * @return a reference to the attached event node factory.
   */
  EventBinding<ExecutionEventNodeFactory> bind(
      Module module,
      CallTarget entryCallTarget,
      Callbacks callbacks,
      Timer timer
  );

  /** A class for notifications about functions being called in the course of execution. */
  final class ExpressionCall {
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
  final class ExpressionValue {
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

  /** Points to the definition of a runtime function. */
  record FunctionPointer(QualifiedName moduleName, QualifiedName typeName, String functionName) {

    public static FunctionPointer fromFunction(Function function) {
      RootNode rootNode = function.getCallTarget().getRootNode();

      QualifiedName moduleName;
      QualifiedName typeName;
      String functionName;

      switch (rootNode) {
        case MethodRootNode methodNode -> {
          moduleName = methodNode.getModuleScope().getModule().getName();
          typeName = methodNode.getType().getQualifiedName();
          functionName = methodNode.getMethodName();
        }
        case QualifiedAccessorNode qualifiedAccessor -> {
          AtomConstructor atomConstructor = qualifiedAccessor.getAtomConstructor();
          moduleName = atomConstructor.getDefinitionScope().getModule().getName();
          typeName = atomConstructor.getType().getQualifiedName();
          functionName = atomConstructor.getName();
        }
        case BuiltinRootNode builtinRootNode -> {
          moduleName = builtinRootNode.getModuleName();
          typeName = builtinRootNode.getTypeName();
          functionName = QualifiedName.fromString(builtinRootNode.getName()).item();
        }
        default -> {
          moduleName = null;
          typeName = null;
          functionName = rootNode.getName();
        }
      }

      return new FunctionPointer(moduleName, typeName, functionName);
    }

    public static int[] collectNotAppliedArguments(Function function) {
      FunctionSchema functionSchema = function.getSchema();
      Object[] preAppliedArguments = function.getPreAppliedArguments();
      boolean isStatic = preAppliedArguments[0] instanceof Type;
      int selfArgumentPosition = isStatic ? -1 : 0;
      int[] notAppliedArguments = new int[functionSchema.getArgumentsCount()];
      int notAppliedArgumentsLength = 0;

      for (int i = 0; i < functionSchema.getArgumentsCount(); i++) {
        if (!functionSchema.hasPreAppliedAt(i)) {
          notAppliedArguments[notAppliedArgumentsLength] = i + selfArgumentPosition;
          notAppliedArgumentsLength += 1;
        }
      }

      return Arrays.copyOf(notAppliedArguments, notAppliedArgumentsLength);
    }
  }

  /** Information about the function call. */
  record FunctionCallInfo(FunctionPointer functionPointer, int[] notAppliedArguments) {

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      FunctionCallInfo that = (FunctionCallInfo) o;
      return Objects.equals(functionPointer, that.functionPointer) && Arrays.equals(
          notAppliedArguments, that.notAppliedArguments);
    }

    @Override
    public int hashCode() {
      int result = Objects.hash(functionPointer);
      return 31 * result + Arrays.hashCode(notAppliedArguments);
    }

    /**
     * Creates a new instance of this record from a function call.
     *
     * @param call the function call.
     */
    public static FunctionCallInfo fromFunctionCall(FunctionCallInstrumentationNode.FunctionCall call) {
      FunctionPointer functionPointer = FunctionPointer.fromFunction(call.getFunction());
      int[] notAppliedArguments = collectNotAppliedArguments(call);

      return new FunctionCallInfo(functionPointer, notAppliedArguments);
    }

    private static int[] collectNotAppliedArguments(FunctionCallInstrumentationNode.FunctionCall call) {
      Object[] arguments = call.getArguments();
      int[] notAppliedArgs = new int[arguments.length];
      int notAppliedArgsSize = 0;
      boolean isStatic = arguments[0] instanceof Type;
      int selfTypePosition = isStatic ? -1 : 0;

      for (int i = 0; i < arguments.length; i++) {
        if (arguments[i] == null) {
          notAppliedArgs[notAppliedArgsSize] = i + selfTypePosition;
          notAppliedArgsSize += 1;
        }
      }

      return Arrays.copyOf(notAppliedArgs, notAppliedArgsSize);
    }
  }
}
