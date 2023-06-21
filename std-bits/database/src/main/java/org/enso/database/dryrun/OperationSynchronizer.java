package org.enso.database.dryrun;

import com.oracle.truffle.api.TruffleLogger;

import java.util.concurrent.ConcurrentLinkedQueue;
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
   * @param action The action is represented by a function taking an integer, because the execution time of
   *     0-argument functions is harder to control. So we add a fake parameter to have full control when the action is
   *     called.
   */
  private record MaintenanceAction(Function<Integer, Object> action) {

    void run() {
      try {
        action.apply(0);
      } catch (Exception e) {
        TruffleLogger.getLogger("enso-std-database").severe("A maintenance action scheduled in the Database library " +
            "failed with an exception: " + e.getMessage());
      }
    }
  }

  private final ConcurrentLinkedQueue<MaintenanceAction> scheduledMaintenanceActions = new ConcurrentLinkedQueue<>();

  /**
   * This lock guarantees that only one thread can access the connection at a time.
   */
  private final ReentrantLock lock = new ReentrantLock();

  /**
   * This counter is used to detect if we are running inside of a regular operation.
   * <p>
   * Maintenance operations are scheduled at safepoints which occur on the main thread, but may interrupt its main task.
   * We do not want maintenance operations to be run while the main thread is in the middle of a regular operation (e.g.
   * transaction), so any maintenance scheduled when this counter is positive will be deferred until after this
   * operation finishes.
   */
  private int regularOperationNestingLevel = 0;

  /**
   * This flag is used to mark that a maintenance operation is currently running and ensure that no other maintenance
   * operation is started until this one finishes.
   * <p>
   * This seems to be redundant, but it is not. When the particular maintenance actions are executed, regular Enso code
   * is run. We have no guarantee that this code will not run additional safepoints while executed - during which
   * another round of maintenance could be started. This flag prevents that overlap.
   */
  private boolean currentlyRunningMaintenance = false;


  /**
   * Runs the provided action ensuring that the current thread is the only one accessing the critical section (in this
   * case: the connection).
   * <p>
   * Due to the usage of re-entry lock, this method may be entered recursively. We exploit that heavily - every
   * 'primitive' operation is wrapped in this, but also 'bulk' actions like transaction are itself wrapped (so that the
   * whole transaction cannot be interrupted).
   * <p>
   * Note that the `runRegularAction` can and will be executed during the maintenance action callbacks - for example
   * when running `execute_update` within it. That is not a problem and is always allowed - the only guarantees that are
   * needed is that when they are run, another 'nested' round of maintenance operation is not started.
   */
  public Object runRegularAction(Function<Integer, Object> action) {
    lock.lock();
    try {
      regularOperationNestingLevel++;
      return action.apply(0);
    } finally {
      regularOperationNestingLevel--;

      /*
       If we have just exited the outer level of regular actions, we should check if any maintenance was scheduled
       during the time we were inside, and run it.
       The `currentlyRunningMaintenance` flag is checked, because during maintenance regular actions may be called as
        well (for example `execute_update`). If we are already inside maintenance, we do not want to start it again
        here.
      */
      if (regularOperationNestingLevel == 0 && !currentlyRunningMaintenance) {
        runScheduledMaintenance();
      }

      lock.unlock();
    }
  }

  public void scheduleMaintenanceAction(Function<Integer, Object> action) {
    scheduledMaintenanceActions.add(new MaintenanceAction(action));

    if (lock.tryLock()) {
      /*
       Once we have a lock, we are guaranteed to be the only thread accessing the connection.
       But we can be running during a safepoint (see ResourceManager) that occurred in the middle of a regular
       operation running, 'interrupting' it.
       So we also check the nesting level and only proceed if its 0.
      */
      try {
        if (regularOperationNestingLevel == 0 && !currentlyRunningMaintenance) {
          /* At this moment we can run the maintenance and we are guaranteed that no other operation on the
          connection will run in parallel to us - we are on the main thread and we are inside of a safepoint
          execution which is not concurrent. */
          runScheduledMaintenance();
        }
      } finally {
        lock.unlock();
      }
    }
  }

  private void runScheduledMaintenance() {
    assert lock.isHeldByCurrentThread();
    assert regularOperationNestingLevel == 0;
    assert !currentlyRunningMaintenance;

    currentlyRunningMaintenance = true;
    try {
      MaintenanceAction actionToRun = null;
      while ((actionToRun = scheduledMaintenanceActions.poll()) != null) {
        actionToRun.run();
      }
    } finally {
      currentlyRunningMaintenance = false;
    }
  }

  /* [Safety]
     ~~~~~~~~
     Firstly, the locks ensure that the operation is run only from within a single thread.
     Due to the safepointing mechanism, this is not enough of a synchronization guarantee, as callbacks run from
     finalizers can interrupt any Enso actions. However, since the safepoint action is run on the thread that holds
     the lock, it will be not run in parallel to it - it can just interrupt it in the middle of some action.
     The counter is used to ensure that maintenance is run only when no regular action (connection operation) is
     currently running. If such an action is interrupted, it will see the counter increased and not run the
     maintenance.
     The flag is used to ensure that any safepoints executed during execution of maintenance itself do not trigger
     an additional 'nested' round of maintenance.
   */

  /* [Liveness]
     ~~~~~~~~~~
     We guarantee that every maintenance action that is scheduled is run at some point.

     There are two scenarios:
     1. The action is scheduled outside of any regular actions. Then it will acquire the lock and check the counter
     and flag and execute itself immediately.
     2. A regular action is running when the action was checking the locks and counter/flag. Then the
     `scheduleMaintenanceAction` will exit immediatelly. But once the regular action exits, it will check that there
     are pending actions and it will run them.

     There should be no risk of an action being scheduled after the regular action exits and before it checks for
     pending actions - see assumption 2.

     Assumptions:
     1. I assume that no other safepoint will interrupt my checks - i.e. I assume there is no safepoint executed
     between `scheduleMaintenanceAction` acquiring the lock and checking the flags.
     2. I assume that safepoint actions occur only in guest languages, so they cannot interrupt the execution of pure
      Java code here unless it explicitly calls-back into Enso.
   */


}
