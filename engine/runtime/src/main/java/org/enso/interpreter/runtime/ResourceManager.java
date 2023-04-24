package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.ThreadLocalAction;
import com.oracle.truffle.api.TruffleSafepoint;
import com.oracle.truffle.api.interop.InteropLibrary;
import org.enso.interpreter.runtime.data.ManagedResource;

import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/** Allows the context to attach garbage collection hooks on the removal of certain objects. */
public class ResourceManager {
  private final EnsoContext context;
  private volatile boolean isClosed = false;
  private volatile Thread workerThread;
  private final Runner worker = new Runner();
  private final ReferenceQueue<ManagedResource> referenceQueue = new ReferenceQueue<>();
  private final ConcurrentMap<PhantomReference<ManagedResource>, Item> items =
      new ConcurrentHashMap<>();

  /**
   * Creates a new instance of Resource Manager.
   *
   * @param context the language context owning the new instance
   */
  public ResourceManager(EnsoContext context) {
    this.context = context;
  }

  /**
   * Puts the finalization of the resource on hold, until {@link #unpark(ManagedResource)} is
   * called. The resource won't be finalized, even if it becomes unreachable between the calls.
   *
   * @param resource the resource to park.
   */
  @CompilerDirectives.TruffleBoundary
  public void park(ManagedResource resource) {
    Item it = items.get(resource.getPhantomReference());
    if (it == null) {
      return;
    }
    it.getParkedCount().incrementAndGet();
  }

  /**
   * Resumes finalization of the resource. If the resource has become unreachable and there are no
   * other threads parking the resource, it will get finalized right away.
   *
   * @param resource the resource to unpark.
   */
  @CompilerDirectives.TruffleBoundary
  public void unpark(ManagedResource resource) {
    Item it = items.get(resource.getPhantomReference());
    if (it == null) {
      return;
    }
    it.getParkedCount().decrementAndGet();
    scheduleFinalizationAtSafepoint(it);
  }

  /**
   * Manually and unconditionally finalizes the resource. Ignores the parking mechanism, assuming
   * the user now has full control over the resource.
   *
   * @param resource the resource to finalize.
   */
  @CompilerDirectives.TruffleBoundary
  public void close(ManagedResource resource) {
    Item it = items.remove(resource.getPhantomReference());
    if (it == null) {
      return;
    }
    // Unconditional finalization – user controls the resource manually.
    it.finalizeNow(context);
  }

  /**
   * Removes the resource from the system, cancelling any automatic finalization action attached to
   * it.
   *
   * @param resource the resource to take away from this system.
   */
  @CompilerDirectives.TruffleBoundary
  public void take(ManagedResource resource) {
    items.remove(resource.getPhantomReference());
  }

  private void scheduleFinalizationAtSafepoint(Item it) {
    if (it.isFlaggedForFinalization().get()) {
      if (it.getParkedCount().get() == 0) {
        // We already know that isFlaggedForFinalization was true at some
        // point and there are no other threads still parking the underlying
        // value. Note that it is impossible for parked count to increase after
        // the value is flagged for finalization, as parking the value requires
        // a live reference. We need to check if another thread didn't reach
        // here earlier to perform the finalization and reset the flag, so that
        // no further attempts are made.
        boolean continueFinalizing = it.isFlaggedForFinalization().compareAndSet(true, false);
        if (continueFinalizing) {
          var futureToCancel = new AtomicReference<Future<Void>>(null);
          var performFinalizeNow =
              new ThreadLocalAction(false, false, true) {
                @Override
                protected void perform(ThreadLocalAction.Access access) {
                  var tmp = futureToCancel.getAndSet(null);
                  if (tmp == null) {
                    return;
                  }
                  tmp.cancel(false);
                  it.finalizeNow(context);
                  items.remove(it.reference);
                }
              };
          futureToCancel.set(context.getEnvironment().submitThreadLocal(null, performFinalizeNow));
        }
      }
    }
  }

  /**
   * Registers a new resource to the system. {@code function} will be called on {@code object} when
   * the value returned by this method becomes unreachable.
   *
   * @param object the underlying resource
   * @param function the finalizer action to call on the underlying resource
   * @return a wrapper object, containing the resource and serving as a reachability probe
   */
  @CompilerDirectives.TruffleBoundary
  public ManagedResource register(Object object, Object function) {
    if (isClosed) {
      throw new IllegalStateException(
          "Can't register new resources after resource manager is closed.");
    }
    if (workerThread == null || !workerThread.isAlive()) {
      worker.setKilled(false);
      workerThread = context.getEnvironment().createSystemThread(worker);
      workerThread.start();
    }
    ManagedResource resource = new ManagedResource(object);
    PhantomReference<ManagedResource> ref = new PhantomReference<>(resource, referenceQueue);
    resource.setPhantomReference(ref);
    items.put(ref, new Item(object, function, ref));
    return resource;
  }

  /**
   * Stops this system, stops and joins any threads created by it. Unconditionally finalizes all the
   * registered resources, ignoring their reachability status.
   *
   * <p>This is only useful when the underlying context is being finalized and no more user code
   * will be run in it.
   */
  public void shutdown() {
    isClosed = true;
    worker.setKilled(true);
    if (workerThread != null) {
      while (true) {
        try {
          workerThread.interrupt();
          workerThread.join();
          break;
        } catch (InterruptedException ignored) {
        }
      }
    }
    for (PhantomReference<ManagedResource> key : items.keySet()) {
      Item it = items.remove(key);
      if (it != null) {
        // Finalize unconditionally – all other threads are dead by now.
        it.finalizeNow(context);
      }
    }
  }

  /**
   * The worker action for the underlying logic of this module. At least one such thread must be
   * spawned in order for this module to be operational.
   */
  private class Runner implements Runnable {
    private volatile boolean killed = false;

    @Override
    public void run() {
      while (true) {
        try {
          Reference<? extends ManagedResource> ref = referenceQueue.remove();
          if (!killed) {
            Item it = items.get(ref);
            if (it == null) {
              continue;
            }
            it.isFlaggedForFinalization().set(true);
            scheduleFinalizationAtSafepoint(it);
          }
          if (killed) {
            return;
          }
        } catch (InterruptedException e) {
          if (killed) {
            return;
          }
        }
      }
    }

    /**
     * Sets the killed flag of this thread. This flag being set to {@code true} will force it to
     * stop execution at the soonest possible safe point. Other than setting this flag, the thread
     * should also be interrupted to read it, in case it is blocked on an interruptible operation.
     *
     * @param killed whether the thread should stop execution upon reading the flag.
     */
    public void setKilled(boolean killed) {
      this.killed = killed;
    }
  }

  /** A storage representation of a finalizable object handled by this system. */
  private static class Item {
    private final Object underlying;
    private final Object finalizer;
    private final PhantomReference<ManagedResource> reference;
    private final AtomicInteger parkedCount = new AtomicInteger();
    private final AtomicBoolean flaggedForFinalization = new AtomicBoolean();

    /**
     * Creates a new finalizable item.
     *
     * @param underlying the underlying object that should be finalized
     * @param finalizer the finalizer to run on the underlying object
     * @param reference a phantom reference used for tracking the reachability status of the
     *     resource.
     */
    public Item(Object underlying, Object finalizer, PhantomReference<ManagedResource> reference) {
      this.underlying = underlying;
      this.finalizer = finalizer;
      this.reference = reference;
    }

    /**
     * Performs the finalization action of this resource right now. The thread must be inside of a
     * context.
     *
     * @param context current execution context
     */
    public void finalizeNow(EnsoContext context) {
      try {
        InteropLibrary.getUncached(finalizer).execute(finalizer, underlying);
      } catch (Exception e) {
        context.getErr().println("Exception in finalizer: " + e.getMessage());
      }
    }

    /**
     * Returns the counter of actions parking this object. The object can be safely finalized only
     * if it's unreachable {@link #isFlaggedForFinalization()} and this counter is zero.
     *
     * @return the parking actions counter
     */
    public AtomicInteger getParkedCount() {
      return parkedCount;
    }

    /**
     * Returns the boolean representing finalization status of this object. The object should be
     * removed by the first thread that observes this flag to be set to true and the {@link
     * #getParkedCount()} to be zero. If a thread intends to perform the finalization, it should set
     * this flag to {@code false}.
     *
     * @return the finalization flag
     */
    public AtomicBoolean isFlaggedForFinalization() {
      return flaggedForFinalization;
    }
  }
}
