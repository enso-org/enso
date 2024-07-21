package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.ThreadLocalAction;
import com.oracle.truffle.api.interop.InteropLibrary;
import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import org.enso.interpreter.runtime.data.ManagedResource;

/** Allows the context to attach garbage collection hooks on the removal of certain objects. */
public final class ResourceManager {
  private final EnsoContext context;
  private volatile boolean isClosed = false;
  private volatile Thread workerThread;
  private final ProcessItems worker = new ProcessItems();
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
  public void park(ManagedResource resource) {
    if (resource.getPhantomReference() instanceof Item it) {
      it.park();
    }
  }

  /**
   * Resumes finalization of the resource. If the resource has become unreachable and there are no
   * other threads parking the resource, it will get finalized right away.
   *
   * @param resource the resource to unpark.
   */
  public void unpark(ManagedResource resource) {
    if (resource.getPhantomReference() instanceof Item it) {
      if (it.unpark(context)) {
        removeFromItems(it);
      }
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Item removeFromItems(PhantomReference<ManagedResource> it) {
    return items.remove(it);
  }

  /**
   * Manually and unconditionally finalizes the resource. Ignores the parking mechanism, assuming
   * the user now has full control over the resource.
   *
   * @param resource the resource to finalize.
   */
  @CompilerDirectives.TruffleBoundary
  public void close(ManagedResource resource) {
    if (resource.getPhantomReference() instanceof Item it) {
      removeFromItems(it);
      // Unconditional finalization – user controls the resource manually.
      it.finalizeNow(context);
    }
  }

  /**
   * Removes the resource from the system, cancelling any automatic finalization action attached to
   * it.
   *
   * @param resource the resource to take away from this system.
   */
  @CompilerDirectives.TruffleBoundary
  public void take(ManagedResource resource) {
    removeFromItems(resource.getPhantomReference());
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
      throw EnsoContext.get(null)
          .raiseAssertionPanic(
              null, "Can't register new resources after resource manager is closed.", null);
    }
    if (workerThread == null || !workerThread.isAlive()) {
      worker.setKilled(false);
      workerThread = context.createThread(true, worker);
      workerThread.start();
    }
    var resource = new ManagedResource(object, r -> new Item(r, object, function, referenceQueue));
    var ref = (Item) resource.getPhantomReference();
    items.put(ref, ref);
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
      Item it = removeFromItems(key);
      if (it != null) {
        // Finalize unconditionally – all other threads are dead by now.
        it.finalizeNow(context);
      }
    }
  }

  /**
   * Processes {@link Item}s eligible for GC. Plays two roles. First of all cleans {@link
   * #referenceQueue} in {@link #run()} method running in its own thread. Then it invokes finalizers
   * in {@link #perform} method inside of Enso execution context.
   */
  private final class ProcessItems extends ThreadLocalAction implements Runnable {
    /**
     * @GuardedBy("pendingItems")
     */
    private final List<Item> pendingItems = new ArrayList<>();

    private Future<Void> request;

    private volatile boolean killed = false;

    ProcessItems() {
      super(false, false, true);
    }

    /**
     * Runs at a safe point in middle of regular Enso program execution. Gathers all available
     * {@link #pendingItems} and runs their finalizers. Removes all processed items from {@link
     * #pendingItems}. If there are any remaining, continues processing them. Otherwise finishes.
     *
     * @param access not used for anything
     */
    @Override
    protected void perform(ThreadLocalAction.Access access) {
      for (; ; ) {
        Item[] toProcess;
        synchronized (pendingItems) {
          request.cancel(false);
          if (pendingItems.isEmpty() || pendingItems.get(0) == null) {
            // nothing to process or already processing
            // avoids re-entrant calls into this method
            return;
          }
          toProcess = pendingItems.toArray(Item[]::new);
          // mark as being processed
          pendingItems.set(0, null);
        }
        try {
          for (var it : toProcess) {
            it.finalizeNow(context);
            removeFromItems(it);
          }
        } finally {
          synchronized (pendingItems) {
            assert pendingItems.size() >= toProcess.length
                : "Just processed "
                    + toProcess.length
                    + " but there is only "
                    + pendingItems.size()
                    + " to clear";
            pendingItems.subList(0, toProcess.length).clear();
          }
        }
      }
    }

    /**
     * Running in its own thread. Waiting for {@link #referenceQueue} to be populated with GCed
     * items. Scheduling {@link #perform} action at safe points while passing the {@link Item}s to
     * it via {@link #pendingItems}.
     */
    @Override
    public void run() {
      while (true) {
        try {
          Reference<? extends ManagedResource> ref = referenceQueue.remove();
          if (!killed) {
            if (ref instanceof Item it) {
              it.flaggedForFinalization.set(true);
              synchronized (pendingItems) {
                if (pendingItems.isEmpty()) {
                  request = context.submitThreadLocal(null, this);
                }
                pendingItems.add(it);
              }
            }
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
    void setKilled(boolean killed) {
      this.killed = killed;
    }
  }

  /** A storage representation of a finalizable object handled by this system. */
  private static final class Item extends PhantomReference<ManagedResource> {
    private final Object underlying;
    private final Object finalizer;

    /**
     * Returns the counter of actions parking this object. The object can be safely finalized only
     * if it's unreachable {@link #isFlaggedForFinalization()} and this counter is zero.
     *
     * @return the parking actions counter
     */
    private final AtomicInteger parkedCount = new AtomicInteger();

    /**
     * Returns the boolean representing finalization status of this object. The object should be
     * removed by the first thread that observes this flag to be set to true and the {@link
     * #getParkedCount()} to be zero. If a thread intends to perform the finalization, it should set
     * this flag to {@code false}.
     *
     * @return the finalization flag
     */
    private final AtomicBoolean flaggedForFinalization = new AtomicBoolean();

    /**
     * Creates a new finalizable item.
     *
     * @param underlying the underlying object that should be finalized
     * @param finalizer the finalizer to run on the underlying object
     * @param reference a phantom reference used for tracking the reachability status of the
     *     resource.
     */
    private Item(
        ManagedResource referent,
        Object underlying,
        Object finalizer,
        ReferenceQueue<ManagedResource> queue) {
      super(referent, queue);
      this.underlying = underlying;
      this.finalizer = finalizer;
    }

    /**
     * Performs the finalization action of this resource right now. The thread must be inside of a
     * context.
     *
     * @param context current execution context
     */
    @CompilerDirectives.TruffleBoundary
    private void finalizeNow(EnsoContext context) {
      try {
        InteropLibrary.getUncached(finalizer).execute(finalizer, underlying);
      } catch (Exception e) {
        context.getErr().println("Exception in finalizer: " + e.getMessage());
      }
    }

    private void park() {
      parkedCount.incrementAndGet();
    }

    /**
     * @return {@code true} if the finalizer was run
     */
    private boolean unpark(EnsoContext context) {
      if (parkedCount.decrementAndGet() == 0) {
        boolean continueFinalizing = flaggedForFinalization.compareAndSet(true, false);
        if (continueFinalizing) {
          finalizeNow(context);
          return true;
        }
      }
      return false;
    }
  }
}
