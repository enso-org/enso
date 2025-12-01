package org.enso.interpreter.instrument;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import org.enso.interpreter.service.GuestExecutionService;
import org.enso.polyglot.RuntimeID;

public class Observable {

  private final Map<UUID, ObservableVisualization> visualizations;
  private final RuntimeID expressionId;

  public Observable(RuntimeID expressionId) {
    this.visualizations = new HashMap<>();
    this.expressionId = expressionId;
  }

  public void registerVisualization(ObservableVisualization visualization) {
    visualizations.put(visualization.visualizationID(), visualization);
  }

  public CompletionStage<Boolean> registerAndRunVisualization(
      ObservableVisualization visualization, Object value, GuestExecutionService executionService) {
    assert value != null;
    visualizations.put(visualization.visualizationID(), visualization);
    return executionService.submitExecution(visualization.execute(value));
  }

  public CompletionStage<Void> runVisualizations(
      GuestExecutionService executionService, Object value) {
    var results =
        visualizations.entrySet().stream()
            .map(
                entry ->
                    executionService
                        .submitExecution(entry.getValue().execute(value))
                        .toCompletableFuture())
            .toList();
    return CompletableFuture.allOf(results.toArray(CompletableFuture[]::new));
  }
}
