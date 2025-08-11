package org.enso.profiling.sampler;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executors;

/** Interface to perform sampling. */
public sealed interface MethodsSampler extends Closeable permits OutputStreamSampler, NoopSampler {
  public static final MethodsSampler NOOP = new NoopSampler();

  /**
   * Create new sampler to write to two output streams.
   *
   * @param npss the sample snapshots or {@code null}
   * @param events associated events or {@code null}
   * @return sampler to use
   * @throws IOException if an I/O operation fails
   */
  public static MethodsSampler create(OutputStream npss, OutputStream events) throws IOException {
    if (npss == null && events == null) {
      return NOOP;
    } else {
      var s1 = npss != null ? npss : OutputStream.nullOutputStream();
      var s2 = events != null ? events : OutputStream.nullOutputStream();
      return new OutputStreamSampler(s1, s2);
    }
  }

  /** Start gathering the application statistics. */
  void start();

  /**
   * Logs an event into events stream.
   *
   * @param at when the message was reported
   * @param message the message to log
   */
  void log(Instant at, String message);

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
