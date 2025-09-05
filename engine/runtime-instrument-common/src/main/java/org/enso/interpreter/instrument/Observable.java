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
import org.enso.interpreter.service.GuestExecutionService;

public class Observable {
  private final Map<UUID, ObservableAction> actions;
  private final Set<Observable> downstreamDependencies;
  private Object value;
  private final UUID uuid;

  public Observable(UUID uuid) {
    this.uuid = uuid;
    this.actions = new ConcurrentHashMap<>();
    this.downstreamDependencies = new HashSet<>();
  }

  // what about cycles?
  public synchronized Stream<UUID> invalidate() {
    this.value = null;
    synchronized (downstreamDependencies) {
      return Stream.concat(
          Stream.of(uuid), this.downstreamDependencies.stream().flatMap(Observable::invalidate));
    }
  }

  public synchronized void update(Object value, GuestExecutionService executionService) {
    update(value, true, executionService);
  }

  public synchronized void update(
      Object value, boolean saveValue, GuestExecutionService executionService) {
    if (saveValue) {
      this.value = value;
    }
    actions
        .values()
        .forEach(
            action -> {
              try {
                executionService.submitExecution(action.execute(value));
              } catch (Throwable e) {
                // FIXME: report/propagate
                e.printStackTrace();
              }
            });
  }

  public Observable register(Observable dependency) {
    if (dependency != null && dependency.uuid != uuid) {
      synchronized (downstreamDependencies) {
        downstreamDependencies.add(dependency);
      }
    } else {
      if (dependency != null && dependency.uuid == uuid) {
        System.out.println(
            "Trying to register observable as a dependency on itself. Investigate " + uuid);
      }
    }
    return this;
  }

  public synchronized CompletionStage<Boolean> registerAction(
      ObservableAction action, GuestExecutionService executionService) {
    // registers an action that will be triggered on update
    // true, if should trigger execution
    actions.put(action.getId(), action);
    if (value != null) {
      return executionService.submitExecution(action.execute(value));
    } else {
      return CompletableFuture.completedStage(false);
    }
  }

  public synchronized boolean deregisterAction(UUID actionId) {
    return actions.remove(actionId) != null;
  }

  public void removeDependency(Observable dependency) {
    // FIXME
  }

  public synchronized Object get() {
    return this.value;
  }

  public synchronized boolean isPending() {
    return value == null;
  }

  @Override
  public String toString() {
    var deps = downstreamDependencies.stream().map(dep -> dep.uuid).collect(Collectors.toSet());
    return "Observable(id="
        + uuid
        + ", dependencies="
        + deps
        + ", actions="
        + actions.keySet()
        + ")";
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof Observable o && o.uuid == uuid;
  }
}
