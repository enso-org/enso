package org.enso.database.dryrun;

import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Function;
import java.util.logging.Logger;
import org.graalvm.polyglot.Value;

/**
 * A helper for ensuring that only one database operation is running at a time, and that maintenance
 * operations requested by finalizers of dry run tables are executed outside of transactions.
 *
 * <p>Additionally, it allows running maintenance actions when no regular actions are being run.
 */
public class OperationSynchronizer {
  /** This lock guarantees that only one thread can access the connection at a time. */
  private final ReentrantLock lock = new ReentrantLock();

  private int nestingLevel = 0;

  /**
   * Runs the provided action ensuring that the current thread is the only one accessing the
   * critical section (in this case: the connection).
   *
   * <p>Due to the usage of re-entry lock, this method may be entered recursively. We exploit that
   * heavily - every 'primitive' operation is wrapped in this, but also 'bulk' actions like
   * transaction are itself wrapped (so that the whole transaction cannot be interrupted).
   *
   * <p>Note: the return type is Value and not Object to preserve Enso specific additional
   * information like warnings or dataflow error; converting to Object could lose some of it.
   */
  public Value runSynchronizedAction(Function<Integer, Value> action) {
    lock.lock();
    try {
      nestingLevel++;
      return action.apply(nestingLevel);
    } finally {
      nestingLevel--;
      lock.unlock();
    }
  }

  /**
   * Runs the provided maintenance action if no regular actions are currently running on this or
   * other threads.
   *
   * <p>If a regular action is currently being executed, this method will exit without doing
   * anything. Conversely, the maintenance action is allowed to run regular synchronized actions
   * inside of it.
   */
  public void runMaintenanceActionIfPossible(Function<Void, Value> maintenanceAction) {
    if (lock.tryLock()) {
      try {
        if (nestingLevel == 0) {
          nestingLevel++;
          try {
            maintenanceAction.apply(null);
          } catch (Exception e) {
            Logger.getLogger("enso-std-database")
                .severe("A maintenance action failed with exception: " + e.getMessage());
          } finally {
            nestingLevel--;
          }
        }
      } finally {
        lock.unlock();
      }
    }
  }
}
