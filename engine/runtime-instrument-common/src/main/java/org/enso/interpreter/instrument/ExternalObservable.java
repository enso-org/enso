package org.enso.interpreter.instrument;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.service.GuestExecutionService;
import org.enso.polyglot.RuntimeID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExternalObservable implements Observable {
  private final Map<UUID, ObservableVisualization> visualizations;
  private final Set<Observable> dependencies;
  private Object value;
  private final RuntimeID id;
  private final boolean isCached;

  private static final Logger LOGGER = LoggerFactory.getLogger(ExternalObservable.class);

  public ExternalObservable(RuntimeID id, boolean isCached) {
    assert id.isExternal();
    this.id = id;
    this.isCached = isCached;
    this.visualizations = new ConcurrentHashMap<>();
    this.dependencies = new HashSet<>();
  }
  

  @Override
  public Observable register(Observable dependency) {
    if (dependency != null && dependency != this) {
      synchronized (dependencies) {
        dependencies.add(dependency);
      }
    }
    return this;
  }

  @Override
  public synchronized Stream<Observable> invalidate() {
    this.value = null;
    synchronized (dependencies) {
      return this.dependencies.stream();
    }
  }

  @Override
  public RuntimeID id() {
    return id;
  }

  @Override
  public synchronized boolean update(Object value, GuestExecutionService executionService) {
    var shouldCacheValue = isCached && !(value instanceof DataflowError);
    if (shouldCacheValue) {
      this.value = value;
    }
    visualizations
        .values()
        .forEach(
            action -> {
              try {
                executionService.submitExecution(action.execute(value));
              } catch (Throwable e) {
                LOGGER.warn(
                    "Failed to submit visualization " + action.getId() + " for execution", e);
              }
            });
    return shouldCacheValue;
  }

  @Override
  public synchronized void notify(Object value, GuestExecutionService executionService) {
    if (value != null) {
      visualizations
          .values()
          .forEach(
              action -> {
                try {
                  executionService.submitExecution(action.execute(value));
                } catch (Throwable e) {
                  LOGGER.warn(
                      "Failed to submit visualization " + action.getId() + " for execution", e);
                }
              });
    }
  }

  public CompletionStage<Boolean> registerAction(
      ObservableVisualization action, GuestExecutionService executionService) {
    visualizations.put(action.getId(), action);
    if (value != null) {
      return executionService.submitExecution(action.execute(value));
    } else {
      return CompletableFuture.completedStage(true);
    }
  }

  public boolean deregisterAction(UUID visualizationId) {
    return visualizations.remove(visualizationId) != null;
  }

  public synchronized Object get() {
    return this.value;
  }

  @Override
  public synchronized void forceVisualizations(GuestExecutionService executionService) {
    if (value != null) {
      visualizations
          .values()
          .forEach(
              action -> {
                try {
                  executionService.submitExecution(action.execute(value));
                } catch (Throwable e) {
                  LOGGER.warn(
                      "Failed to submit visualization " + action.getId() + " for execution", e);
                }
              });
    }
  }

  @Override
  public boolean isExternal() {
    return true;
  }

  @Override
  public String toString() {
    var deps = dependencies.stream().map(Observable::id).collect(Collectors.toSet());
    return "Observable(id="
        + id
        + ", direct dependencies="
        + deps
        + ", visualizations="
        + visualizations.keySet()
        + ", value="
        + (value != null ? "non-empty" : "empty")
        + ")";
  }

  @Override
  public boolean hasDependency(RuntimeID id) {
    return dependencies.stream().anyMatch(o -> o.id().equals(id));
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof ExternalObservable o && o.id == id;
  }
}
