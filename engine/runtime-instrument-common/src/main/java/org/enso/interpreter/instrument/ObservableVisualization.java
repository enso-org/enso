package org.enso.interpreter.instrument;

import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Supplier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A wrapper class that encapsulates visualization to be executed whenever a required value is
 * provided.
 */
public class ObservableVisualization {
  private final Consumer<Object> action;
  private final UUID visualizationId;
  private static final Logger LOGGER = LoggerFactory.getLogger(ObservableVisualization.class);

  /**
   * Creates a new {@link ObservableVisualization}
   *
   * @param visualizationId unique visualization id
   * @param action visualization taking a value of the expression it is assigned to
   */
  public ObservableVisualization(UUID visualizationId, Consumer<Object> action) {
    this.action = action;
    this.visualizationId = visualizationId;
  }

  public UUID getId() {
    return visualizationId;
  }

  /**
   * Returns a supplier consuming the value of the {@link Observable} it is assigned to, ready to be
   * executed by the execution service.
   *
   * @param value underlying value of assigned {@link Observable}
   * @return a supplier encapsulating visualization ready to be executed
   */
  public Supplier<Boolean> execute(Object value) {
    return () -> {
      try {
        action.accept(value);
        return true;
      } catch (Throwable e) {
        LOGGER.warn("Failed to execute visualization " + visualizationId, e);
        return false;
      }
    };
  }
}
