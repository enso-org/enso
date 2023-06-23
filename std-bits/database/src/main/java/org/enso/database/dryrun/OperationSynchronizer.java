package org.enso.database.dryrun;

import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Function;

/**
 * A helper for ensuring that only one database operation is running at a time, and that maintenance operations
 * requested by finalizers of dry run tables are executed outside of transactions.
 * <p>
 * Additionally, it allows scheduling a maintenance operation to be executed. This request can be fired on any thread
 * and the class guarantees that the maintenance will run _outside_ of any synchronized critical sections.
 */
public class OperationSynchronizer {
  /**
   * This lock guarantees that only one thread can access the connection at a time.
   */
  private final ReentrantLock lock = new ReentrantLock();

  private int nestingLevel = 0;

  public record ActionResult(Object result, int nestingLevel) {
  }

  /**
   * Runs the provided action ensuring that the current thread is the only one accessing the critical section (in this
   * case: the connection).
   * <p>
   * Due to the usage of re-entry lock, this method may be entered recursively. We exploit that heavily - every
   * 'primitive' operation is wrapped in this, but also 'bulk' actions like transaction are itself wrapped (so that the
   * whole transaction cannot be interrupted).
   * <p>
   * It returns a pair containing the action result and the nesting level at which the action was executed.
   */
  public ActionResult runSynchronizedAction(Function<Integer, Object> action) {
    lock.lock();
    try {
      int nestingLevelToReport = nestingLevel;

      nestingLevel++;
      var result = action.apply(0);
      return new ActionResult(result, nestingLevelToReport);
    } finally {
      nestingLevel--;
      lock.unlock();
    }
  }
}
