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
   *
   * @deprecated This method should not be used until the bug <a
   *     href="https://github.com/enso-org/enso/issues/7117">#7117</a> is fixed.
   */
  @Deprecated
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
   * Enters a synchronized action, incrementing the nesting level.
   *
   * <p>This is much less safe than `runSynchronizedAction` but it gets us around the <a
   * href="https://github.com/enso-org/enso/issues/7117">#7117</a> bug.
   *
   * <p>Any call to this MUST be followed by a call to `exitSynchronizedAction` in a `finally`
   * block, or otherwise the resource may be locked forever.
   */
  public int enterSynchronizedAction() {
    lock.lock();
    nestingLevel++;
    return nestingLevel;
  }

  /**
   * Exits a synchronized action, decrementing the nesting level.
   *
   * @see #enterSynchronizedAction()
   */
  public void exitSynchronizedAction() {
    nestingLevel--;
    lock.unlock();
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
