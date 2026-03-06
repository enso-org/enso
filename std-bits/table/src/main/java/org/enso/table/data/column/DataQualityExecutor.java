package org.enso.table.data.column;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Supplier;

final class DataQualityExecutor {
  private DataQualityExecutor() {}

  // A thread pool for executing data quality metrics computations asynchronously.
  private static final ExecutorService threadFactory =
      Executors.newFixedThreadPool(Math.min(4, Runtime.getRuntime().availableProcessors() / 2));

  static <T> CompletableFuture<T> supplyAsync(Supplier<T> action) {
    return CompletableFuture.supplyAsync(action, threadFactory);
  }
}
