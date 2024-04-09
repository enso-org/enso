package org.enso.polyglot.debugger;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.interop.TruffleObject;
import java.util.UUID;

public interface IdExecutionService {
  String INSTRUMENT_ID = "id-value-extractor";

  public abstract class Info {

    /**
     * @return UUID of the node, never {@code null}.
     */
    public abstract UUID getId();

    /**
     * @return associated result or {@code null} if there is no associated result.
     */
    public abstract Object getResult();

    /**
     * @return {@code true} when the result is panic, {@code false} otherwise.
     */
    public abstract boolean isPanic();

    /**
     * @return time (in nanoseconds) needed to compute the result or {@code -1} when not available.
     */
    public abstract long getElapsedTime();

    /**
     * Evaluates given code in the context of current UUID location.
     *
     * @param code the Enso code to evaluate.
     * @return result of the evaluation.
     */
    public abstract Object eval(String code);
  }

  public interface Callbacks {

    /**
     * Finds out previously computed result for given id. If a result is returned, then the
     * execution of given node is skipped and the value is returned back.
     *
     * @param info info with UUID the node to be computed
     * @return {@code null} should the execution of the node be performed; any other value to skip
     *     the execution and return the value as a result.
     */
    Object findCachedResult(Info info);

    /**
     * Notifies when an execution of a node is over.
     *
     * @param info info with node id, {@link Info#getResult()}, {@link Info#isPanic()} and {@link
     *     Info#getElapsedTime()}
     */
    void updateCachedResult(Info info);

    /**
     * Notification when a returned value is a function.
     *
     * @param info with identification of the node and {@link Info#getResult()} info about function
     *     call
     * @return {@code null} should the execution of the node be performed; any other value to skip
     *     the execution and return the value as a result.
     */
    Object onFunctionReturn(Info info);
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
