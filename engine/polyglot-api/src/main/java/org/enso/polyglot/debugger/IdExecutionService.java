package org.enso.polyglot.debugger;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.interop.TruffleObject;
import java.util.UUID;

public interface IdExecutionService {
  String INSTRUMENT_ID = "id-value-extractor";

  public interface Callbacks {
    /**
     * Finds out previously computed result for given id. If a result is returned, then the
     * execution of given node is skipped and the value is returned back.
     *
     * @param nodeId identification of the node to be computed
     * @return {@code null} should the execution of the node be performed; any other value to skip
     *     the execution and return the value as a result.
     */
    Object findCachedResult(UUID nodeId);

    /**
     * Notifies when an execution of a node is over.
     *
     * @param nodeId identification of the node to be computed
     * @param result the just computed result
     * @param isPanic was the result a panic?
     * @param nanoElapsedTime how long it took to compute the result?
     */
    void updateCachedResult(UUID nodeId, Object result, boolean isPanic, long nanoElapsedTime);

    /**
     * Notification when a returned value is a function.
     *
     * @param nodeId identification of the node to be computed
     * @param result info about function call
     * @return {@code null} should the execution of the node be performed; any other value to skip
     *     the execution and return the value as a result.
     */
    Object onFunctionReturn(UUID nodeId, TruffleObject result);
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
      TruffleObject module, CallTarget entryCallTarget, Callbacks callbacks, Object timer);
}
