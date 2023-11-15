package org.enso.profiling.sampler;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

public interface MethodsSampler {

  /** Start gathering the application statistics. */
  void start();

  /** Stop gathering the application statistics and write it to the output. */
  void stop() throws IOException;

  /**
   * Stop gathering the application statistics after the provided delay and write it to the output.
   *
   * @param delay the duration to wait before stopping
   * @param unit a unit determining how to interpret the delay parameter
   * @param executor the executor
   */
  default CompletableFuture<Void> scheduleStop(long delay, TimeUnit unit, Executor executor) {
    Executor delayedExecutor = CompletableFuture.delayedExecutor(delay, unit, executor);
    return CompletableFuture.runAsync(
        () -> {
          try {
            this.stop();
          } catch (IOException e) {
            throw new CompletionException(e);
          }
        },
        delayedExecutor);
  }
}
