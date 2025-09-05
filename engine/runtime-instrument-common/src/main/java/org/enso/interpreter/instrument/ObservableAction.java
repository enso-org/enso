package org.enso.interpreter.instrument;

import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class ObservableAction {
  private final Consumer<Object> action;
  private final UUID visualizationId;

  public ObservableAction(UUID visualizationId, Consumer<Object> action) {
    this.action = action;
    this.visualizationId = visualizationId;
  }

  public UUID getId() {
    return visualizationId;
  }

  public Supplier<Boolean> execute(Object value) {
    return () -> {
      try {
        action.accept(value);
        return true;
      } catch (Throwable e) {
        // FIXME: proper reporting
        e.printStackTrace();
        return false;
      }
    };
  }

  public void stop() {
    // FIXME: should disassociate from any Observables that it was assigned to
  }
}
