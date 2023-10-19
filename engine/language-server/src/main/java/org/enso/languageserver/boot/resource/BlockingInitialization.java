package org.enso.languageserver.boot.resource;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Semaphore;

public final class BlockingInitialization implements InitializationComponent {

  private final InitializationComponent component;
  private final Semaphore lock = new Semaphore(1);

  public BlockingInitialization(InitializationComponent component) {
    this.component = component;
  }

  @Override
  public boolean isInitialized() {
    return component.isInitialized();
  }

  @Override
  public CompletableFuture<Void> init() {
    try {
      lock.acquire();
    } catch (InterruptedException e) {
      return CompletableFuture.failedFuture(e);
    }
    return component.init().whenComplete((res, err) -> lock.release());
  }
}
