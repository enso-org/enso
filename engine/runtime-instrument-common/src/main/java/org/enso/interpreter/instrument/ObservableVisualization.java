package org.enso.interpreter.instrument;

import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Supplier;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public record ObservableVisualization(Consumer<Object> action, UUID visualizationID) {

  private static final Logger LOGGER = LoggerFactory.getLogger(ObservableVisualization.class);

  public Supplier<Boolean> execute(Object value) {
    return () -> {
      try {
        action.accept(value);
        return true;
      } catch (Throwable e) {
        LOGGER.warn("Failed to execute visualization " + visualizationID, e);
        return false;
      }
    };
  }
}
