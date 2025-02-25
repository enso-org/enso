package org.enso.table.data.column.operation;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicReference;
import org.slf4j.Logger;

public class CachedPropertyCheck<T> {

  private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(CachedPropertyCheck.class);

  private final AtomicReference<CompletableFuture<T>> cachedFuture;

  // A lambda that computes the value,
  // example is the RequiresNumberFormatting computable future in numeric storages.
  private final Computation<T> computation;
  private final T defaultValue;

  @FunctionalInterface
  public interface Computation<T> {
    T compute() throws Exception;
  }

  public CachedPropertyCheck(Computation<T> computation, T defaultValue) {
    this.computation = computation;
    this.defaultValue = defaultValue;

    // Initially, compute the value and cache it.
    this.cachedFuture =
        new AtomicReference<>(
            CompletableFuture.supplyAsync(
                () -> {
                  try {
                    return computation.compute();
                  } catch (Exception e) {
                    throw new RuntimeException(e);
                  }
                }));
  }

  /**
   * Returns the cached value, recomputing it if the previous computation was cancelled.
   *
   * @return the computed value
   * @throws InterruptedException if the current thread was interrupted while waiting
   */
  public T get() throws InterruptedException {
    CompletableFuture<T> future = cachedFuture.get();
    if (future.isCancelled()) {
      // Recompute if the previous computation was cancelled.
      future =
          CompletableFuture.supplyAsync(
              () -> {
                try {
                  return computation.compute();
                } catch (Exception e) {
                  throw new RuntimeException(e);
                }
              });
      cachedFuture.set(future);
    }
    try {
      return future.get();
    } catch (ExecutionException e) {
      LOGGER.error("Failed to compute cached value", e);
      return defaultValue;
    }
  }
}
