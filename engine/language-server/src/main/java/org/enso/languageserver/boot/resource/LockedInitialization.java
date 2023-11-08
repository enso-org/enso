package org.enso.languageserver.boot.resource;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.Semaphore;

public abstract class LockedInitialization implements InitializationComponent {
  private final Executor executor;
  private final Semaphore lock = new Semaphore(1);
  private boolean isInitialized;

  protected LockedInitialization(Executor executor) {
    this.executor = executor;
    this.isInitialized = false;
  }

  @Override
  public boolean isInitialized() {
    return isInitialized;
  }

  public abstract void initComponent();

  @Override
  public CompletableFuture<Void> init() {
    return CompletableFuture.runAsync(
        () -> {
          if (!isInitialized) {
            try {
              lock.acquire();
            } catch (InterruptedException e) {
              e.printStackTrace();
              return;
            }
            try {
              if (!isInitialized) {
                initComponent();
                isInitialized = true;
              }
            } finally {
              lock.release();
            }
          }
        },
        executor);
  }
}
