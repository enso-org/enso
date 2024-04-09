package org.enso.languageserver.boot.resource;

import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;

/** Initializes resources in sequence. */
public class SequentialResourcesInitialization implements InitializationComponent {

  private final InitializationComponent[] resources;
  private final Executor executor;

  /**
   * Create an instance of sequential initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param resources the list of resources to initialize
   */
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
