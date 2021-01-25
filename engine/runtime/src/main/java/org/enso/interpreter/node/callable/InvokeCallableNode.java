package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.source.SourceSection;
import java.util.UUID;
import java.util.concurrent.locks.Lock;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.NotInvokableException;
import org.enso.interpreter.runtime.state.Stateful;

/**
 * This class is responsible for performing the actual invocation of a given callable with its
 * arguments.
 *
 * <p>This invocation is segmented via the type of the callable, allowing handling of the various
 * different kinds within a seamless framework.
 */
public abstract class InvokeCallableNode extends BaseNode {

  /** Denotes the mode of defaulted arguments treatment for a function invocation. */
  public enum DefaultsExecutionMode {
    /** Defaulted arguments should be ignored for this application position. */
    IGNORE,
    /** Defaulted arguments should be executed normally for this application position. */
    EXECUTE;

    /**
     * Should the presence of defaulted arguments be ignored for this application position?
     *
     * @return {@code true} if the defaulted arguments should be ignored, {@code false} otherwise
     */
    public boolean isIgnore() {
      return this == IGNORE;
    }

    /**
     * Whether or not any default arguments be executed in this application position.
     *
     * @return {@code true} if the defaulted arguments should be executed, {@code false} otherwise
     */
    public boolean isExecute() {
      return this == EXECUTE;
    }
  }

  /** Denotes the mode of arguments execution for a function invocation */
  public enum ArgumentsExecutionMode {
    /** Arguments are pre-executed for this call. */
    PRE_EXECUTED,
    /**
     * Arguments are passed as {@link Thunk}s and should be executed before calling the function.
     */
    EXECUTE;

    /**
     * Should the arguments be executed before calling the function?
     *
     * @return {@code true} if the arguments should be executed, {@code false} otherwise
     */
    public boolean shouldExecute() {
      return this == EXECUTE;
    }
  }

  @Child private InvokeFunctionNode invokeFunctionNode;
  @Child private InvokeMethodNode invokeMethodNode;
  @Child private ThunkExecutorNode thisExecutor;
  private final ConditionProfile functionErrorProfile = ConditionProfile.createCountingProfile();

  private final boolean canApplyThis;
  private final int thisArgumentPosition;

  private final ArgumentsExecutionMode argumentsExecutionMode;

  InvokeCallableNode(
      CallArgumentInfo[] schema,
      DefaultsExecutionMode defaultsExecutionMode,
      ArgumentsExecutionMode argumentsExecutionMode) {
    Integer thisArg = thisArgumentPosition(schema);

    this.canApplyThis = thisArg != null;
    this.thisArgumentPosition = thisArg == null ? 0 : thisArg;

    this.argumentsExecutionMode = argumentsExecutionMode;

    this.invokeFunctionNode =
        InvokeFunctionNode.build(schema, defaultsExecutionMode, argumentsExecutionMode);
    this.invokeMethodNode =
        InvokeMethodNode.build(schema, defaultsExecutionMode, argumentsExecutionMode);
  }

  public static Integer thisArgumentPosition(CallArgumentInfo[] schema) {
    int idx = 0;
    for (; idx < schema.length; idx++) {
      CallArgumentInfo arg = schema[idx];

      boolean isNamedThis = arg.isNamed() && arg.getName().equals(Constants.Names.THIS_ARGUMENT);
      if (arg.isPositional() || isNamedThis) {
        return idx;
      }
    }
    return null;
  }

  /**
   * Creates a new instance of this node.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param defaultsExecutionMode the defaulted arguments handling mode for this call
   * @param argumentsExecutionMode the arguments execution mode for this call
   */
  public static InvokeCallableNode build(
      CallArgumentInfo[] schema,
      DefaultsExecutionMode defaultsExecutionMode,
      ArgumentsExecutionMode argumentsExecutionMode) {
    return InvokeCallableNodeGen.create(schema, defaultsExecutionMode, argumentsExecutionMode);
  }

  @Specialization
  Stateful invokeFunction(
      Function function, VirtualFrame callerFrame, Object state, Object[] arguments) {
    return this.invokeFunctionNode.execute(function, callerFrame, state, arguments);
  }

  @Specialization
  Stateful invokeConstructor(
      AtomConstructor constructor, VirtualFrame callerFrame, Object state, Object[] arguments) {
    return invokeFunction(constructor.getConstructorFunction(), callerFrame, state, arguments);
  }

  @Specialization
  public Stateful invokeDynamicSymbol(
      UnresolvedSymbol symbol, VirtualFrame callerFrame, Object state, Object[] arguments) {
    if (canApplyThis) {
      Object selfArgument = arguments[thisArgumentPosition];
      if (argumentsExecutionMode.shouldExecute()) {
        if (thisExecutor == null) {
          CompilerDirectives.transferToInterpreterAndInvalidate();
          Lock lock = getLock();
          lock.lock();
          try {
            if (thisExecutor == null) {
              thisExecutor = insert(ThunkExecutorNode.build());
            }
          } finally {
            lock.unlock();
          }
        }
        Stateful selfResult = thisExecutor.executeThunk(selfArgument, state, TailStatus.NOT_TAIL);
        selfArgument = selfResult.getValue();
        state = selfResult.getState();
        arguments[thisArgumentPosition] = selfArgument;
      }
      return invokeMethodNode.execute(callerFrame, state, symbol, selfArgument, arguments);
    } else {
      throw new RuntimeException("Currying without `this` argument is not yet supported.");
    }
  }

  @Fallback
  public Stateful invokeGeneric(
      Object callable, VirtualFrame callerFrame, Object state, Object[] arguments) {
    throw new NotInvokableException(callable, this);
  }

  /**
   * Executes the provided {@code callable} on the supplied {@code arguments}.
   *
   * @param callable the callable to evaluate
   * @param callerFrame the caller frame to pass to the function
   * @param state the state to pass to the function
   * @param arguments the arguments to evaluate {@code callable} on
   * @return the result of executing {@code callable} on the supplied {@code arguments}
   */
  public abstract Stateful execute(
      Object callable, VirtualFrame callerFrame, Object state, Object[] arguments);

  /**
   * Sets whether or not the current node is tail-recursive.
   *
   * @param isTail whether or not the node is tail-recursive.
   */
  @Override
  public void setTailStatus(TailStatus isTail) {
    super.setTailStatus(isTail);
    invokeFunctionNode.setTailStatus(isTail);
    invokeMethodNode.setTailStatus(isTail);
  }

  /** @return the source section for this node. */
  @Override
  public SourceSection getSourceSection() {
    Node parent = getParent();
    return parent == null ? null : parent.getSourceSection();
  }

  /**
   * Sets the expression ID of this node.
   *
   * @param id the ID to assign this node.
   */
  public void setId(UUID id) {
    invokeFunctionNode.setId(id);
    invokeMethodNode.setId(id);
  }
}
