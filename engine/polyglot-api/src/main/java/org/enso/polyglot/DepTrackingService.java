package org.enso.polyglot;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.interop.TruffleObject;

public interface DepTrackingService {
  String INSTRUMENT_ID = "dep-tracking";

  public interface Callbacks {

    /**
     * Mark the beginning of the execution of the body of the assignment.
     *
     * <p>Any local variable reads will be executed within the context of {@code runtimeID} and will
     * be marked as upstream dependencies of {@code runtimeID}. Once processing of the node is done,
     * {@link #endVariableAssignment(RuntimeID)} must be called.
     *
     * @param runtimeID identifier of the node
     */
    void startVariableAssignment(RuntimeID runtimeID);

    /**
     * Marks the end of the execution of the body of the assignment.
     *
     * @param runtimeID identifier of the node
     */
    void endVariableAssignment(RuntimeID runtimeID);

    /**
     * If result of the execution of the node is a local variable read, then it is registered as a
     * dependency of the currently executed context.
     *
     * @param runtimeID identifier of the current
     * @param result result of executing the node
     * @return {@code result} stripped from runtime tracking information, if necessary
     */
    Object registerReturnValue(RuntimeID runtimeID, Object result);

    /**
     * Wraps {@code value} in a corresponding {@link Ref} object.
     *
     * @param runtimeID identifier of the current node
     * @param value TruffleObject
     * @return {@code value} wrapped in an object that carries runtime dependency info
     */
    Object wrapAsReference(RuntimeID runtimeID, Object value);
  }

  /**
   * Attach a new event node factory to observe runtime dependencies between local variables.
   *
   * @param module module that contains the code
   * @param entryCallTarget the call target being observed.
   * @param timer the execution timer.
   * @return a reference to the attached event node factory.
   */
  EventBinding<ExecutionEventNodeFactory> bind(
      TruffleObject module, Callbacks callbacks, CallTarget entryCallTarget, Object timer);
}
