package org.enso.base_test_helpers;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class LoggerHelper {
  private static final System.Logger LOG = System.getLogger(LoggerHelper.class.getName());
  private static final ExecutorService EXEC =
      Executors.newSingleThreadExecutor(
          (r) -> {
            return Thread.ofPlatform().name("LoggerHelper").daemon(true).start(r);
          });

  public static boolean isLoggableAsync(System.Logger.Level level) throws Exception {
    return EXEC.submit(() -> LOG.isLoggable(level)).get();
  }

  public static void logAsync(System.Logger.Level level, String msg) throws Exception {
    EXEC.submit(() -> LOG.log(level, msg)).get();
  }
}
