package org.enso.languageserver.boot.resource;

import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;

public class SequentialResourcesInitialization implements InitializationComponent {

  private final InitializationComponent[] resources;
  private final Executor executor;

  public SequentialResourcesInitialization(
      Executor executor, InitializationComponent... resources) {
    this.resources = resources;
    this.executor = executor;
  }

  @Override
  public boolean isInitialized() {
    return Arrays.stream(resources).allMatch(InitializationComponent::isInitialized);
  }

  @Override
  public CompletableFuture<Void> init() {
    CompletableFuture<Void> result = CompletableFuture.completedFuture(null);

    for (InitializationComponent component : resources) {
      result =
          result.thenComposeAsync(
              res ->
                  component.isInitialized()
                      ? CompletableFuture.completedFuture(null)
                      : component.init(),
              executor);
    }

    return result;
  }
}
