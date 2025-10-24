package org.enso.interpreter.instrument;

import java.util.UUID;
import java.util.concurrent.CompletionStage;
import java.util.stream.Stream;
import org.enso.interpreter.service.GuestExecutionService;
import org.enso.polyglot.RuntimeID;

public interface Observable {

  /**
   * Registers another observable as being dependent on this one. Recursive dependencies are not
   * allowed.
   *
   * @param observable another observable
   * @return this {@link Observable}
   */
  Observable register(Observable observable);

  /**
   * A stream of direct {@link Observable} that are dependent on this {@link Observable}.
   * Invalidation also removed cached values associated with this observable, if any.
   *
   * @return a stream of direct observable dependencies
   */
  Stream<Observable> invalidate();

  /**
   * ID of the node associated with this {@link Observable}.
   *
   * @return UUID of the node
   */
  RuntimeID id();

  /**
   * Returns a cached value associated with this {@link Observable}, if available. If this
   * Observable is not caching values, it always returns {@code null}.
   *
   * @return cached value, if available
   */
  Object get();

  /**
   * Indicates if values associated with this {@link Observable} can be cached, and therefore
   * whether visualizations can be registered with this observable.
   *
   * @return {@code true}, if values can being cached, {@code false} otherwise
   */
  default boolean isExternal() {
    return false;
  }

  default void forceVisualizations(GuestExecutionService executionService) {}
  ;

  /**
   * Updates the underlying value carried by the observable, if the observable supports it. {@code
   * DataflowError} is never cached.
   *
   * @param value value to be assocaited with this observable
   * @param executionService execution service to use for executing any visualizations
   * @return
   */
  default boolean update(Object value, GuestExecutionService executionService) {
    throw new UnsupportedOperationException("Observable.update is unsupported");
  }

  /**
   * Runs any visualizations associated with this Observable, without caching the value.
   *
   * @param value value to be passed to any associated visualizations
   * @param executionService execution service to use for executing any visualizations
   */
  default void notify(Object value, GuestExecutionService executionService) {
    throw new UnsupportedOperationException("Observable.notify is unsupported");
  }

  /**
   * Associates {@link ObservableVisualization} with this {@link Observable}. Action will be
   * triggered whenever a new value is recorded. An instance of {@link GuestExecutionService} is
   * needed in the case a value is already recorded at the time the action is being registered. In
   * such case, the action can be executed immediately. If {@see isCached()} returns {@code false},
   * an exception is thrown.
   *
   * @param action visualization to be executed with the evaluated value
   * @param executionService execution service to use for executing the visualization, if possible
   * @return future indicating if the registration/execution of visualization has completed
   */
  default CompletionStage<Boolean> registerAction(
      ObservableVisualization action, GuestExecutionService executionService) {
    throw new UnsupportedOperationException(
        "Observable.registerAction is unsupported in " + this.getClass());
  }

  /**
   * Disassociates {@link ObservableVisualization} from this {@link Observable}.
   *
   * @param visualizationId id of the visualization
   * @return {@code true} if successful, {@code false} otherwise
   */
  default boolean deregisterAction(UUID visualizationId) {
    throw new UnsupportedOperationException(
        "Observable.deregisterAction is unsupported in " + this.getClass());
  }

  /**
   * Creates a new instance of {@link Observable} for the given ID.
   *
   * @param id unique identifier of the node
   * @return new instance of {@link Observable}
   */
  static Observable fromUUID(RuntimeID id) {
    if (id.isExternal()) return new ExternalObservable(id, id.isCached());
    else return new InternalObservable(id);
  }

  /**
   * Checks if this Observable has an upstream dependency with a given ID.
   *
   * @param id unique identifier to check
   * @return true if dependency is present, false otherwise
   */
  boolean hasDependency(RuntimeID id);
}
