package org.enso.base_test_helpers;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class LoggerHelper {
  private static final System.Logger LOG = System.getLogger(LoggerHelper.class.getName());
  private static final ExecutorService EXEC =
      Executors.newSingleThreadExecutor(
          (r) -> {
            var t = new Thread(r, "LoggerHelper");
            t.setDaemon(true);
            return t;
          });

  public static boolean isLoggableAsync(System.Logger.Level level) throws Exception {
    final Callable<Boolean> action =
        () -> {
          return LOG.isLoggable(level);
        };
    return EXEC.submit(action).get();
  }

  public static void logAsync(System.Logger.Level level, String msg) throws Exception {
    final Callable<Void> action =
        () -> {
          LOG.log(level, msg);
          return null;
        };
    EXEC.submit(action).get();
  }
}
