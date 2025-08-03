package org.enso.profiling.sampler;

import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executors;

/** Interface to perform sampling. */
public sealed interface MethodsSampler extends Closeable permits OutputStreamSampler, NoopSampler {
  public static final MethodsSampler NOOP = new NoopSampler();

  /**
   * Create new sampler to write to two files.
   *
   * @param npss the sample snapshots
   * @param events associated events
   * @return sampler to use
   * @throws FileNotFoundException if an I/O operation fails
   */
  public static MethodsSampler create(File npss, File events) throws FileNotFoundException {
    return new OutputStreamSampler(new FileOutputStream(npss), new FileOutputStream(events));
  }

  /** Start gathering the application statistics. */
  void start();

  /**
   * Logs an event into events stream.
   *
   * @param message the message to log
   */
  void log(String message);

  /**
   * Stop gathering the application statistics after the provided delay and write it to the output.
   *
   * @param delay the duration to wait before stopping
   * @return future to check the status of the
   */
  default CompletableFuture<Void> scheduleStop(Duration delay) {
    var vThread = Executors.newVirtualThreadPerTaskExecutor();
    return CompletableFuture.runAsync(
        () -> {
          try {
            Thread.sleep(delay.toNanos());
            close();
          } catch (InterruptedException | IOException e) {
            throw new CompletionException(e);
          }
        },
        vThread);
  }
}
